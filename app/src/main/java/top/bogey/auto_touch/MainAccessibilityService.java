package top.bogey.auto_touch;

import android.accessibilityservice.AccessibilityService;
import android.content.Intent;
import android.util.Log;
import android.view.accessibility.AccessibilityEvent;
import android.view.accessibility.AccessibilityNodeInfo;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.data.TaskRepository;
import top.bogey.auto_touch.room.data.TaskRunnable;
import top.bogey.auto_touch.util.CompleteCallback;

public class MainAccessibilityService extends AccessibilityService {
    private final ExecutorService findService = Executors.newFixedThreadPool(2);
    private final ExecutorService configService = Executors.newFixedThreadPool(5);
    private final List<TaskRunnable> tasks = new ArrayList<>();

    private TaskRepository repository;
    private String currPkgName = "";

    public boolean enable = false;

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
    }

    @Override
    public boolean onUnbind(Intent intent) {
        MainApplication.setService(null);
        return super.onUnbind(intent);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        MainApplication.setService(this);
        if (repository == null){
            repository = new TaskRepository(getApplicationContext());
        }
        return super.onStartCommand(intent, flags, startId);
    }

    public void runTask(Task task, CompleteCallback callback){
        if (enable){
            TaskRunnable taskRunnable = new TaskRunnable(this, task, callback);
            tasks.add(taskRunnable);
            configService.submit(taskRunnable);
        } else {
            if (callback != null){
                callback.onComplete();
            }
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
                        if (task.isStopped()) task.stop();
                    }
                    tasks.clear();

                    currPkgName = packageName;
                    // APP自己不执行任何任务
                    if (packageName.equals(getPackageName())) return;

                    MainActivity activity = MainApplication.getActivity();
                    if (activity != null){
                        activity.dismissPlayView();
                    }

                    Log.d("TAG", packageName);

                    boolean isCommon = false;
                    List<Task> tasks = repository.getTasksByPackageName(packageName);
                    if (tasks == null || tasks.isEmpty()){
                        tasks = repository.getTasksByPackageName(getString(R.string.common_package_name));
                        isCommon = true;
                    }
                    if (tasks == null || tasks.isEmpty()) return;

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
                        }
                    }
                }
            }
        }
    }
}
