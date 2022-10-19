package top.bogey.touch_tool;

import android.accessibilityservice.AccessibilityService;
import android.accessibilityservice.GestureDescription;
import android.app.Activity;
import android.app.job.JobInfo;
import android.app.job.JobScheduler;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.graphics.Path;
import android.os.IBinder;
import android.os.PersistableBundle;
import android.util.Log;
import android.view.accessibility.AccessibilityEvent;

import androidx.lifecycle.MutableLiveData;

import com.tencent.mmkv.MMKV;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.room.data.FindRunnable;
import top.bogey.touch_tool.room.data.TaskCallable;
import top.bogey.touch_tool.room.data.TaskRepository;
import top.bogey.touch_tool.utils.ResultCallback;
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

    private int jobId = 1000;


    public MainAccessibilityService() {
        findService = Executors.newFixedThreadPool(2);
        taskService = new ThreadPoolExecutor(3, 20, 60L, TimeUnit.SECONDS, new ArrayBlockingQueue<>(20));
    }

    @Override
    public void onAccessibilityEvent(AccessibilityEvent event) {
        if (event != null && isServiceEnabled()){
            if (event.getEventType() == AccessibilityEvent.TYPE_WINDOWS_CHANGED){
                if (findRunnable != null) findRunnable.stop();
                findRunnable = new FindRunnable(this, tasks, currPkgName);
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

        JobScheduler jobScheduler = (JobScheduler) getSystemService(Context.JOB_SCHEDULER_SERVICE);
        jobScheduler.cancelAll();

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

        JobScheduler jobScheduler = (JobScheduler) getSystemService(Context.JOB_SCHEDULER_SERVICE);
        jobScheduler.cancelAll();
    }

    public boolean isServiceConnected() {
        return serviceConnected;
    }

    public void setServiceEnabled(boolean enabled){
        serviceEnabled.setValue(enabled);
        MMKV.defaultMMKV().encode(SERVICE_ENABLED, enabled);

        if (isServiceEnabled()){
            List<Task> tasks = TaskRepository.getInstance(this).getTasksByStatus(TaskStatus.TIME);
            for (Task task : tasks) {
                addJob(task);
            }
        } else {
            JobScheduler jobScheduler = (JobScheduler) getSystemService(Context.JOB_SCHEDULER_SERVICE);
            jobScheduler.cancelAll();
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

    public TaskCallable runTask(Task task, TaskCallback callback){
        if (task == null || !isServiceEnabled()) return null;
        TaskCallable callable = new TaskCallable(this, task, currPkgName, callback);
        tasks.add(callable);
        taskService.submit(callable);
        return callable;
    }

    public void stopTask(TaskCallable callable){
        if (callable == null || !isServiceEnabled()) return;
        tasks.remove(callable);
        callable.stop(true);
        try {
            taskService.invokeAny(Collections.singletonList(callable));
        } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void addJob(Task task){
        if (task == null || !isServiceEnabled()) return;
        removeJob(task);
        long timeMillis = System.currentTimeMillis();
        if (task.getStatus() == TaskStatus.TIME && task.getTime() > timeMillis) {
            JobScheduler jobScheduler = (JobScheduler) getSystemService(Context.JOB_SCHEDULER_SERVICE);
            PersistableBundle bundle = new PersistableBundle();
            bundle.putString("id", task.getId());
            JobInfo jobInfo = new JobInfo.Builder(jobId++, new ComponentName(getPackageName(), MainJobService.class.getName()))
                    .setMinimumLatency(task.getTime() - timeMillis)
                    .setOverrideDeadline(task.getTime() - timeMillis + 1000)
                    .setExtras(bundle)
                    .build();

            Log.d("TAG", "addJob: " + (task.getTime() - timeMillis));
            if (jobScheduler.schedule(jobInfo) > 0){
                Log.d("TAG", "addJob: " + task.getTitle() + task.getId());
            }
        }
    }

    public void removeJob(Task task){
        if (task == null || !isServiceEnabled()) return;
        JobScheduler jobScheduler = (JobScheduler) getSystemService(Context.JOB_SCHEDULER_SERVICE);
        for (JobInfo jobInfo : jobScheduler.getAllPendingJobs()) {
            PersistableBundle extras = jobInfo.getExtras();
            if (task.getId().equals(extras.getString("id"))){
                jobScheduler.cancel(jobInfo.getId());
                break;
            }
        }
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
