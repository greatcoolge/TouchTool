package top.bogey.auto_touch;

import android.accessibilityservice.AccessibilityService;
import android.accessibilityservice.GestureDescription;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.graphics.Path;
import android.os.IBinder;
import android.util.Log;
import android.view.accessibility.AccessibilityEvent;
import android.view.accessibility.AccessibilityNodeInfo;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.data.TaskRepository;
import top.bogey.auto_touch.room.data.TaskRunnable;
import top.bogey.auto_touch.util.ResultCallback;
import top.bogey.auto_touch.util.RunningCallback;

public class MainAccessibilityService extends AccessibilityService {
    private static final String SAVE_PATH = "Save";
    private static final String SERVICE_ENABLED = "service_enabled";
    private static final String DEBUG_TIPS_ENABLED = "debug_tips_enabled";

    private final ExecutorService findService;
    public final ExecutorService taskService;
    private final List<TaskRunnable> tasks = new ArrayList<>();

    private final List<GestureDescription.StrokeDescription> strokes = new LinkedList<>();
    private boolean gestureRunning = false;

    private TaskRepository repository;
    private String currPkgName = "";

    private boolean enable = false;
    public boolean connected = false;

    private ServiceConnection serviceConnection;
    public CaptureService.CaptureBinder binder;
    private ResultCallback resultCallback;

    public MainAccessibilityService() {
        findService = Executors.newFixedThreadPool(4);
        taskService = new ThreadPoolExecutor(3, 20, 60L, TimeUnit.SECONDS, new ArrayBlockingQueue<>(20));
    }

    @Override
    public void onAccessibilityEvent(AccessibilityEvent event) {
        if (event != null && enable && repository != null){
            if (event.getEventType() == AccessibilityEvent.TYPE_WINDOWS_CHANGED){
                findService.execute(new FindRunnable());
            }
        }
    }

    @Override
    public void onInterrupt() { }

    @Override
    protected void onServiceConnected() {
        super.onServiceConnected();
        MainApplication.setService(this);
        connected = true;

        SharedPreferences sharedPreferences = getSharedPreferences(SAVE_PATH, Context.MODE_PRIVATE);
        enable = sharedPreferences.getBoolean(SERVICE_ENABLED, false);

        if (repository == null){
            repository = new TaskRepository(this);
        }
    }

    @Override
    public boolean onUnbind(Intent intent) {
        MainApplication.setService(null);
        connected = false;
        return super.onUnbind(intent);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        MainApplication.setService(this);
        if (repository == null){
            repository = new TaskRepository(this);
        }
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        connected = false;
        MainApplication.setService(null);
        stopCaptureService();
        repository = null;
    }

    public boolean isEnable() {
        return enable;
    }

    public void setEnable(boolean enable) {
        this.enable = enable;

        SharedPreferences sharedPreferences = getSharedPreferences(SAVE_PATH, Context.MODE_PRIVATE);
        SharedPreferences.Editor edit = sharedPreferences.edit();
        edit.putBoolean(SERVICE_ENABLED, enable);
        edit.apply();
    }

    public static boolean isShowDebugTips(Context context){
        SharedPreferences sharedPreferences = context.getSharedPreferences(SAVE_PATH, Context.MODE_PRIVATE);
        return sharedPreferences.getBoolean(DEBUG_TIPS_ENABLED, false);
    }

    public static void setShowDebugTips(Context context, boolean isShow){
        SharedPreferences sharedPreferences = context.getSharedPreferences(SAVE_PATH, Context.MODE_PRIVATE);
        SharedPreferences.Editor edit = sharedPreferences.edit();
        edit.putBoolean(DEBUG_TIPS_ENABLED, isShow);
        edit.apply();
    }

    public TaskRunnable runTask(Task task, RunningCallback callback){
        if (enable){
            TaskRunnable taskRunnable = new TaskRunnable(this, task, callback);
            tasks.add(taskRunnable);
            taskService.submit(taskRunnable);
            return taskRunnable;
        } else {
            if (callback != null){
                callback.onResult(false);
            }
            return null;
        }
    }

    public void runGesture(Path path, int time){
        strokes.add(new GestureDescription.StrokeDescription(path, 0, time));
        runGesture();
    }

    private void runGesture(){
        if (!gestureRunning){
            if (strokes.size() > 0){
                gestureRunning = true;
                GestureDescription.StrokeDescription strokeDescription = strokes.remove(0);
                dispatchGesture(new GestureDescription.Builder().addStroke(strokeDescription).build(), new GestureResultCallback() {
                    @Override
                    public void onCompleted(GestureDescription gestureDescription) {
                        super.onCompleted(gestureDescription);
                        gestureRunning = false;
                        runGesture();
                    }

                    @Override
                    public void onCancelled(GestureDescription gestureDescription) {
                        super.onCancelled(gestureDescription);
                        gestureRunning = false;
                        runGesture();
                    }
                }, null);
            }
        }
    }

    public void startCaptureService(boolean moveBack, ResultCallback callback){
        if (binder == null){
            resultCallback = callback;
            serviceConnection = new ServiceConnection() {
                @Override
                public void onServiceConnected(ComponentName name, IBinder service) {
                    binder = (CaptureService.CaptureBinder) service;
                }

                @Override
                public void onServiceDisconnected(ComponentName name) {
                    binder = null;
                }
            };
            Intent intent = new Intent(this, CaptureService.class);
            intent.putExtra("MoveBack", moveBack);
            bindService(intent, serviceConnection, Context.BIND_AUTO_CREATE);
        } else {
            if (callback != null) callback.onResult(true);
        }
    }

    public void stopCaptureService(){
        if (serviceConnection != null){
            unbindService(serviceConnection);
            stopService(new Intent(this, CaptureService.class));
            serviceConnection = null;
            binder = null;
        }
    }

    public void callCaptureServiceResult(boolean result){
        if (!result){
            stopCaptureService();
        }
        if (resultCallback != null){
            resultCallback.onResult(result);
        }
    }

    private class FindRunnable implements Runnable{
        private int times = 0;

        @Override
        public void run() {
            AccessibilityNodeInfo nodeInfo = getRootInActiveWindow();
            while (nodeInfo == null){
                nodeInfo = getRootInActiveWindow();
                times++;
                // 1秒内查找当前窗口
                if (times > 20){
                    break;
                }
                if (nodeInfo == null){
                    try {
                        Thread.sleep(50);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
            if (nodeInfo != null){
                String packageName = String.valueOf(nodeInfo.getPackageName());
                // 同一个包在未切换过窗口时不会再执行新的任务
                if (!(packageName.equals("null") || packageName.equals(currPkgName))){
                    // 取消所有非当前包下的任务
                    for (TaskRunnable task : tasks) {
                        if (task.isRunning()) task.stop();
                    }
                    tasks.clear();

                    currPkgName = packageName;

                    MainActivity activity = MainApplication.getActivity();
                    if (activity != null){
                        activity.dismissPlayView();
                    }

                    // APP自己不执行任何任务
                    if (packageName.equals(getPackageName())) return;

                    Log.d("EnterApp", packageName);

                    List<Task> tasks = getAppTaskByPkgName(packageName);

                    String comPkgName = getString(R.string.common_package_name);
                    boolean isManual = false;
                    for (Task task : tasks) {
                        if (task.getActions() != null && !task.getActions().isEmpty()){
                            switch (task.getTaskStatus()) {
                                case AUTO:
                                    runTask(task, null);
                                    break;
                                case MANUAL:
                                    String name = task.getPkgName();
                                    if (!name.equals(comPkgName)){
                                        isManual = true;
                                    }
                                    break;
                            }
                        }
                    }
                    if (isManual){
                        if (activity != null){
                            activity.showPlayView(packageName);
                        } else {
                            Intent intent = new Intent(MainAccessibilityService.this, MainActivity.class);
                            intent.putExtra("IsBackground", true);
                            intent.putExtra("FloatPackageName", packageName);
                            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                            startActivity(intent);
                        }
                    }
                }
            }
        }

        private List<Task> getAppTaskByPkgName(String pkgName){
            List<Task> tasks = new ArrayList<>();
            List<Task> pkgTasks = repository.getTasksByPackageNames(pkgName);
            if (pkgTasks != null){
                tasks.addAll(pkgTasks);
            }
            String conPkgName = getString(R.string.common_package_name);
            List<Task> comTasks = repository.getTasksByPackageNames(conPkgName);
            if (comTasks != null){
                for (Task comTask : comTasks) {
                    boolean flag = true;
                    for (Task task : tasks) {
                        if (comTask.getTitle().equals(task.getTitle())){
                            flag = false;
                            break;
                        }
                    }
                    if (flag){
                        tasks.add(comTask);
                    }
                }
            }
            return tasks;
        }
    }
}
