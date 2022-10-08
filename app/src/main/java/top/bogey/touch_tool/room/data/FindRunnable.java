package top.bogey.touch_tool.room.data;

import android.content.Intent;
import android.view.accessibility.AccessibilityNodeInfo;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.RunningUtils;

public class FindRunnable implements Runnable{
    private boolean isRunning = true;

    private final MainAccessibilityService service;
    private final List<TaskCallable> tasks;
    private final String pkgName;
    public FindRunnable(MainAccessibilityService service, List<TaskCallable> tasks, String pkgName) {
        this.service = service;
        this.tasks = tasks;
        this.pkgName = pkgName;
    }

    public void stop() {
        isRunning = false;
    }

    public boolean isRunning(){
        return isRunning;
    }

    private void sleep(){
        try {
            Thread.sleep(50);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void run() {
        AccessibilityNodeInfo root = service.getRootInActiveWindow();
        int times = 0;
        while (root == null && isRunning()){
            times++;
            if (times > 20) break;
            sleep();
            root = service.getRootInActiveWindow();
        }
        if (root != null && isRunning()){
            String packageName = String.valueOf(root.getPackageName());
            if (!(packageName.equals("null") || packageName.equals(pkgName))){
                RunningUtils.log(service, LogLevel.MIDDLE, service.getString(R.string.log_surface_changed, pkgName));

                // 取消所有非当前包下的普通任务
                for (TaskCallable task : tasks) {
                    if (task.isRunning()) task.stop();
                }
                tasks.clear();
                service.currPkgName = packageName;

                MainActivity activity = MainApplication.getActivity();
                if (activity != null){
                    activity.dismissPlayFloatView();
                }

                // APP自己不执行任何任务
                if (packageName.equals(service.getPackageName())) return;

                RunningUtils.log(service, LogLevel.MIDDLE, service.getString(R.string.log_surface_entered, packageName));

                List<Task> tasks = getAppTaskByPkgName(packageName);

                if (!isRunning()) return;

                RunningUtils.log(service, LogLevel.LOW, service.getString(R.string.log_get_all_task, tasks.size()));

                boolean isManual = false;
                for (Task task : tasks) {
                    if (task.getActions() != null && !task.getActions().isEmpty()){
                        switch (task.getStatus()) {
                            case AUTO:
                                service.runTask(task, null);
                                RunningUtils.log(service, LogLevel.MIDDLE, service.getString(R.string.log_run_auto_task, task.getTitle()));
                                break;
                            case MANUAL:
                                isManual = true;
                                break;
                        }
                    }
                }
                if (isManual){
                    if (activity != null){
                        activity.showPlayFloatView(packageName);
                    } else {
                        Intent intent = new Intent(service, MainActivity.class);
                        intent.putExtra("IsBackground", true);
                        intent.putExtra("FloatPackageName", packageName);
                        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                        service.startActivity(intent);
                    }
                }
            }
        }
    }

    private List<Task> getAppTaskByPkgName(String pkgName){
        TaskRepository repository = new TaskRepository(service);
        List<Task> tasks = new ArrayList<>();
        List<Task> pkgTasks = repository.getTasksByPackageName(pkgName);
        if (pkgTasks != null){
            tasks.addAll(pkgTasks);
        }
        String conPkgName = service.getString(R.string.common_package_name);
        List<Task> comTasks = repository.getTasksByPackageName(conPkgName);
        if (comTasks != null){
            for (Task comTask : comTasks) {
                boolean flag = true;
                for (Task task : tasks) {
                    if (task.getTitle() != null && comTask.getTitle().equals(task.getTitle())){
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
