package top.bogey.touch_tool.database.data;

import android.content.Intent;
import android.view.accessibility.AccessibilityNodeInfo;

import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskType;
import top.bogey.touch_tool.ui.play.OverseeMode;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.LogUtils;
import top.bogey.touch_tool.ui.setting.SettingSave;

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
                service.currPkgName = packageName;

                MainActivity activity = MainApplication.getActivity();
                // APP自己不执行任何任务
                if (packageName.equals(service.getPackageName())) {
                    if (activity != null) activity.dismissPlayFloatView();
                    service.stopAllTask(false);
                    return;
                }

                // 切换应用时执行
                if (!packageName.equals(pkgName)) {
                    LogUtils.log(LogLevel.MIDDLE, service.getString(R.string.log_run_app_changed, pkgName, packageName));
                    service.stopTaskByType(TaskType.APP_CHANGED, false);
                    service.stopTaskByType(TaskType.CONTENT_CHANGED, false);
                    // 应用切换，开启内容检测看看有没有检测的任务
                    service.setContentEvent(true);

                    List<Task> tasks = service.getAllTasksByPkgNameAndType(packageName, TaskType.APP_CHANGED);
                    for (Task task : tasks) {
                        if (task.getBehaviors() != null && !task.getBehaviors().isEmpty()) {
                            service.runTask(task, null);
                            LogUtils.log(LogLevel.MIDDLE, service.getString(R.string.log_run_app_changed_task, task.getTitle()));
                        }
                    }

                    tasks = service.getAllTasksByPkgNameAndType(packageName, TaskType.MANUAL);
                    if (tasks.size() > 0 || SettingSave.getInstance().getRunningOverseeMode() != OverseeMode.CLOSED) {
                        if (activity != null) {
                            activity.showPlayFloatView(packageName);
                        } else {
                            Intent intent = new Intent(service, MainActivity.class);
                            intent.putExtra(MainActivity.INTENT_KEY_BACKGROUND, true);
                            intent.putExtra(MainActivity.INTENT_KEY_PLAY_PACKAGE, packageName);
                            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                            service.startActivity(intent);
                        }
                    } else {
                        if (activity != null) activity.dismissPlayFloatView();
                    }
                }

                service.stopTaskByType(TaskType.VIEW_CHANGED, false);
                List<Task> tasks = service.getAllTasksByPkgNameAndType(packageName, TaskType.VIEW_CHANGED);
                if (tasks.size() > 0) LogUtils.log(LogLevel.MIDDLE, service.getString(R.string.log_run_view_changed, pkgName));
                for (Task task : tasks) {
                    if (task.getType() == TaskType.CONTENT_CHANGED && task.getBehaviors() != null && !task.getBehaviors().isEmpty()) {
                        // 窗口变动时执行
                        service.runTask(task, null);
                        LogUtils.log(LogLevel.MIDDLE, service.getString(R.string.log_run_view_changed_task, task.getTitle()));
                    }
                }
            }
        }
    }
}
