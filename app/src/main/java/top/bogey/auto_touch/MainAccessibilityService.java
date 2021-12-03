package top.bogey.auto_touch;

import android.accessibilityservice.AccessibilityService;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;
import android.view.accessibility.AccessibilityEvent;
import android.view.accessibility.AccessibilityNodeInfo;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.data.TaskRepository;
import top.bogey.auto_touch.room.data.TaskRunnable;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.CompleteCallback;

public class MainAccessibilityService extends AccessibilityService {
    private final ExecutorService findService = Executors.newFixedThreadPool(2);
    private final ExecutorService configService = Executors.newFixedThreadPool(5);
    private final List<TaskRunnable> tasks = new ArrayList<>();

    private TaskRepository repository;
    private String currPkgName = "";

    public boolean enable = false;
    public boolean connected = false;

    private ServiceConnection serviceConnection;
    public CaptureService.CaptureBinder binder;

    @Override
    public void onAccessibilityEvent(AccessibilityEvent event) {
        if (event != null && enable){
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
            repository = new TaskRepository(getApplication());
        }
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        connected = false;
        MainApplication.setService(null);
        stopCaptureService();
    }

    public TaskRunnable runTask(Task task, CompleteCallback callback){
        if (enable){
            TaskRunnable taskRunnable = new TaskRunnable(this, task, callback);
            tasks.add(taskRunnable);
            configService.submit(taskRunnable);
            return taskRunnable;
        } else {
            if (callback != null){
                callback.onComplete();
            }
            return null;
        }
    }

    public void startCaptureService(boolean moveBack, CompleteCallback callback){
        if (binder == null){
            serviceConnection = new ServiceConnection() {
                @Override
                public void onServiceConnected(ComponentName name, IBinder service) {
                    binder = (CaptureService.CaptureBinder) service;
                    if (callback != null) callback.onComplete();
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
            if (callback != null) callback.onComplete();
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
                        AppUtil.log("Dismiss Dialog", "");
                    }

                    // APP自己不执行任何任务
                    if (packageName.equals(getPackageName())) return;

                    AppUtil.log("EnterApp", packageName);

                    boolean isCommon = false;
                    List<Task> tasks = repository.getTasksByPackageName(currPkgName);
                    if (tasks == null || tasks.isEmpty()){
                        tasks = repository.getTasksByPackageName(getString(R.string.common_package_name));
                        isCommon = true;
                    }
                    if (tasks == null || tasks.isEmpty()) return;
                    AppUtil.log("Handler tasks", "");

                    String pkgName = "";
                    for (Task task : tasks) {
                        switch (task.taskStatus) {
                            case AUTO:
                                runTask(task, null);
                                break;
                            case MANUAL:
                                pkgName = task.pkgName;
                                break;
                        }
                    }
                    if (!(isCommon || pkgName.isEmpty())){
                        if (activity != null){
                            activity.showPlayView(pkgName);
                            AppUtil.log("Show Dialog", pkgName);
                        }
                    }
                }
            }
        }
    }
}
