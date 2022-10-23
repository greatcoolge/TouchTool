package top.bogey.touch_tool;

import android.accessibilityservice.AccessibilityService;
import android.accessibilityservice.GestureDescription;
import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.graphics.Path;
import android.os.IBinder;
import android.view.accessibility.AccessibilityEvent;

import androidx.lifecycle.MutableLiveData;
import androidx.work.Data;
import androidx.work.ExistingPeriodicWorkPolicy;
import androidx.work.ExistingWorkPolicy;
import androidx.work.OneTimeWorkRequest;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;

import com.tencent.mmkv.MMKV;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.room.data.FindRunnable;
import top.bogey.touch_tool.room.data.TaskCallable;
import top.bogey.touch_tool.room.data.TaskRepository;
import top.bogey.touch_tool.room.data.TaskWorker;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.RunningUtils;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.ResultCallback;
import top.bogey.touch_tool.utils.RunStateCallback;
import top.bogey.touch_tool.utils.TaskCallback;

public class MainAccessibilityService extends AccessibilityService {
    private static final String SERVICE_ENABLED = "service_enabled";

    // 服务
    private boolean serviceConnected = false;
    public static final MutableLiveData<Boolean> serviceEnabled = new MutableLiveData<>(false);

    // 截屏
    public static final MutableLiveData<Boolean> captureEnabled = new MutableLiveData<>(false);
    public MainCaptureService.CaptureServiceBinder binder = null;
    private ServiceConnection connection = null;

    // 任务
    public String currPkgName = "";
    private final ExecutorService findService;
    private FindRunnable findRunnable = null;
    public final ExecutorService taskService;
    private final List<TaskCallable> tasks = new ArrayList<>();
    private final List<RunStateCallback> runStates = new ArrayList<>();

    public MainAccessibilityService() {
        findService = Executors.newFixedThreadPool(2);
        taskService = new ThreadPoolExecutor(3, 20, 60L, TimeUnit.SECONDS, new ArrayBlockingQueue<>(20));
    }

    @Override
    public void onAccessibilityEvent(AccessibilityEvent event) {
        if (event != null && isServiceEnabled()){
            if (event.getEventType() == AccessibilityEvent.TYPE_WINDOWS_CHANGED){
                if (findRunnable != null) findRunnable.stop();
                findRunnable = new FindRunnable(this, currPkgName);
                findService.execute(findRunnable);
            }
        }
    }

    @Override
    public void onInterrupt() {}

    @Override
    protected void onServiceConnected() {
        super.onServiceConnected();
        serviceConnected = true;
        MainApplication.setService(this);

        setServiceEnabled(MMKV.defaultMMKV().decodeBool(SERVICE_ENABLED, false));
    }

    @Override
    public boolean onUnbind(Intent intent) {
        serviceConnected = false;
        MainApplication.setService(null);
        stopCaptureService();

        WorkManager.getInstance(this).cancelAllWork();

        return super.onUnbind(intent);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        MainApplication.setService(this);
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        serviceConnected = false;
        MainApplication.setService(null);
        stopCaptureService();

        WorkManager.getInstance(this).cancelAllWork();
    }

    public boolean isServiceConnected() {
        return serviceConnected;
    }

    public void setServiceEnabled(boolean enabled){
        serviceEnabled.setValue(enabled);
        MMKV.defaultMMKV().encode(SERVICE_ENABLED, enabled);

        if (isServiceEnabled()){
            List<Task> tasks = TaskRepository.getInstance(this).getTasksByStatus(TaskStatus.TIME);
            if (tasks != null){
                for (Task task : tasks) {
                    addWork(task);
                }
            }
        } else {
            WorkManager.getInstance(this).cancelAllWork();
        }
    }

    public boolean isServiceEnabled(){
        return isServiceConnected() && Boolean.TRUE.equals(serviceEnabled.getValue());
    }

    public void startCaptureService(boolean moveBack, ResultCallback callback){
        if (binder == null){
            MainActivity activity = MainApplication.getActivity();
            if (activity != null){
                activity.launchCapture(((code, data) -> {
                    if (code == Activity.RESULT_OK){
                        connection = new ServiceConnection(){
                            @Override
                            public void onServiceConnected(ComponentName name, IBinder service) {
                                binder = (MainCaptureService.CaptureServiceBinder) service;
                                captureEnabled.setValue(true);
                                if (moveBack) activity.moveTaskToBack(true);
                                if (callback != null) callback.onResult(true);
                            }

                            @Override
                            public void onServiceDisconnected(ComponentName name) {
                                stopCaptureService();
                            }
                        };
                        Intent intent = new Intent(this, MainCaptureService.class);
                        intent.putExtra("Data", data);
                        boolean result = bindService(intent, connection, Context.BIND_AUTO_CREATE);
                        if (!result) if (callback != null) callback.onResult(false);
                    } else {
                        if (callback != null) callback.onResult(false);
                    }
                }));
            } else {
                if (callback != null) callback.onResult(false);
            }
        } else {
            if (callback != null) callback.onResult(true);
        }
    }

    public void stopCaptureService(){
        if (connection != null){
            unbindService(connection);
            stopService(new Intent(this, MainCaptureService.class));
            connection = null;
        }
        binder = null;
        captureEnabled.setValue(false);
    }

    public boolean isCaptureEnabled(){
        return isServiceConnected() && Boolean.TRUE.equals(captureEnabled.getValue());
    }

    public List<TaskCallable> getRunningTasks(RunStateCallback callback){
        if (!runStates.contains(callback)) runStates.add(callback);
        return tasks;
    }

    public TaskCallable runTask(Task task, TaskCallback callback){
        if (task == null || !isServiceEnabled()) return null;

        TaskCallable callable = new TaskCallable(this, task, currPkgName);
        callable.setCallback(new TaskCallback() {
            @Override
            public void onStart() {
                if (callback != null) callback.onStart();
                tasks.add(callable);
                for (int i = runStates.size() - 1; i >= 0; i--) {
                    RunStateCallback runStateCallback = runStates.get(i);
                    if (runStateCallback != null) runStateCallback.onNewTask(callable);
                    else runStates.remove(i);
                }
            }

            @Override
            public void onEnd(boolean succeed) {
                if (callback != null) callback.onEnd(succeed);

                for (int i = runStates.size() - 1; i >= 0; i--) {
                    RunStateCallback runStateCallback = runStates.get(i);
                    if (runStateCallback != null) runStateCallback.onTaskEnd(callable);
                    else runStates.remove(i);
                }

                synchronized (tasks) {
                    tasks.remove(callable);
                }

            }

            @Override
            public void onProgress(int percent) {
                if (callback != null) callback.onProgress(percent);
                for (int i = runStates.size() - 1; i >= 0; i--) {
                    RunStateCallback runStateCallback = runStates.get(i);
                    if (runStateCallback != null) runStateCallback.onTaskProgress(callable, percent);
                    else runStates.remove(i);
                }
            }
        });
        taskService.submit(callable);
        return callable;
    }

    public void stopTask(TaskCallable callable){
        stopTask(callable, false);
    }

    public void stopTask(TaskCallable callable, boolean force){
        if (callable == null || !isServiceEnabled()) return;
        callable.stop(force);
    }

    public void stopAllTask(boolean force) {
        synchronized (tasks){
            for (int i = tasks.size() - 1; i >= 0; i--) {
                stopTask(tasks.get(i), force);
            }
        }
    }

    public void addWork(Task task){
        if (task == null || !isServiceEnabled()) return;
        long timeMillis = System.currentTimeMillis();
        if (task.getStatus() == TaskStatus.TIME) {
            WorkManager workManager = WorkManager.getInstance(this);
            if (task.getPeriodic() > 0){
                // 尽量小延迟的执行间隔任务
                PeriodicWorkRequest workRequest = new PeriodicWorkRequest.Builder(TaskWorker.class, task.getPeriodic(), TimeUnit.MINUTES, 5, TimeUnit.MINUTES)
                        .setInitialDelay(task.getTime() - timeMillis, TimeUnit.MILLISECONDS)
                        .setInputData(new Data.Builder()
                                .putString("id", task.getId())
                                .putString("title", task.getTitle())
                                .build())
                        .build();
                workManager.enqueueUniquePeriodicWork(task.getId(), ExistingPeriodicWorkPolicy.REPLACE, workRequest);
                RunningUtils.log(LogLevel.MIDDLE, getString(R.string.log_add_periodic_job, task.getTitle(), AppUtils.formatDateMinute(task.getTime()), task.getPeriodic() / 60, task.getPeriodic() % 60));
            } else if(task.getTime() > timeMillis) {
                OneTimeWorkRequest workRequest = new OneTimeWorkRequest.Builder(TaskWorker.class)
                        .setInitialDelay(task.getTime() - timeMillis, TimeUnit.MILLISECONDS)
                        .setInputData(new Data.Builder()
                                .putString("id", task.getId())
                                .putString("title", task.getTitle())
                                .build())
                        .build();
                workManager.enqueueUniqueWork(task.getId(), ExistingWorkPolicy.REPLACE, workRequest);
                RunningUtils.log(LogLevel.MIDDLE, getString(R.string.log_add_job, task.getTitle(), AppUtils.formatDateMinute(task.getTime())));
            } else {
                // 所有条件都未达到，任务无效，尝试移除已有的任务
                removeWork(task);
            }
        }
    }

    public void removeWork(Task task){
        if (task == null || !isServiceEnabled()) return;
        WorkManager workManager = WorkManager.getInstance(this);
        workManager.cancelUniqueWork(task.getId());
        RunningUtils.log(LogLevel.MIDDLE, getString(R.string.log_remove_job, task.getTitle()));
    }

    public void runGesture(Path path, int time, ResultCallback callback){
        dispatchGesture(new GestureDescription.Builder().addStroke(new GestureDescription.StrokeDescription(path, 0, time)).build(), new GestureResultCallback() {
            @Override
            public void onCompleted(GestureDescription gestureDescription) {
                super.onCompleted(gestureDescription);
                if (callback != null) callback.onResult(true);
            }

            @Override
            public void onCancelled(GestureDescription gestureDescription) {
                super.onCancelled(gestureDescription);
                if (callback != null) callback.onResult(false);
            }
        }, null);
    }
}
