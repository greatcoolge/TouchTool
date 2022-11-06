package top.bogey.touch_tool.database.data;

import android.content.Intent;
import android.view.accessibility.AccessibilityNodeInfo;

import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.RunningUtils;

public class FindRunnable implements Runnable {
    private boolean isRunning = true;

    private final MainAccessibilityService service;
    private final String pkgName;

    public FindRunnable(MainAccessibilityService service, String pkgName) {
        this.service = service;
        this.pkgName = pkgName;
    }

    public void stop() {
        isRunning = false;
    }

    public boolean isRunning() {
        return isRunning;
    }

    private void sleep() {
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
        while (root == null && isRunning()) {
            times++;
            if (times > 20) break;
            sleep();
            root = service.getRootInActiveWindow();
        }
        if (root != null && isRunning()) {
            String packageName = String.valueOf(root.getPackageName());
            if (!"null".equals(packageName)) {
                // 尝试停止任务并取消所有非运行中任务
                service.stopAllTask(false);

                // 关闭手动任务悬浮窗
                MainActivity activity = MainApplication.getActivity();
                if (activity != null) {
                    activity.dismissPlayFloatView();
                }

                // APP自己不执行任何任务
                if (packageName.equals(service.getPackageName())) return;

                // 获取所有能在这个应用下运行的任务
                List<Task> tasks = getAppTaskByPkgName(packageName);

                if (!isRunning()) return;

                RunningUtils.log(LogLevel.LOW, service.getString(R.string.log_get_all_task, tasks.size()));

                boolean isManual = false;

                for (Task task : tasks) {
                    if (task.getBehaviors() != null && !task.getBehaviors().isEmpty()) {
                        switch (task.getType()) {
                            case MANUAL:
                                isManual = true;
                                break;
                            case APP_CHANGED:
                                // 应用第一次开启时执行，相当于没切换过应用
                                if (!packageName.equals(pkgName)){
                                    service.runTask(task, null);
                                }
                                break;
                            case VIEW_CHANGED:
                                // 界面变动时执行
                                service.runTask(task, null);
                                break;
                        }
                    }
                }

                service.currPkgName = packageName;

                if (isManual) {
                    if (activity != null) {
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

    private List<Task> getAppTaskByPkgName(String pkgName) {
        List<Task> comTasks = TaskRepository.getInstance().getTasksByPkgName(service.getString(R.string.common_package_name));
        List<Task> pkgTasks = TaskRepository.getInstance().getTasksByPkgName(pkgName);
        comTasks.addAll(pkgTasks);
        return comTasks;
    }
}
