package top.bogey.touch_tool.database.data;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.ActionType;
import top.bogey.touch_tool.database.bean.action.TaskAction;
import top.bogey.touch_tool.utils.TaskRunningCallback;

public class TaskRunningInfo implements TaskRunningCallback {
    private final List<TaskRunningCallback> callbacks = new ArrayList<>();

    private final String pkgName;
    private final int maxProgress;
    private final TaskRunnable runnable;

    private boolean isRunning = true;
    private int progress = 0;

    public TaskRunningInfo(TaskRunnable runnable, String pkgName, int maxProgress) {
        this.runnable = runnable;
        this.pkgName = pkgName;
        this.maxProgress = maxProgress;
    }

    public String getPkgName() {
        return pkgName;
    }

    public boolean isRunning() {
        return isRunning;
    }

    public void setRunning(boolean running) {
        if (isRunning) isRunning = running;
    }

    public synchronized void addProgress(Task task, Action action, boolean skip) {
        if (action.getType() != ActionType.TASK) {
            progress++;
        } else {
            if (skip){
                Task subTask = task.getSubTaskById(((TaskAction) action).getId());
                if (subTask != null) progress += subTask.getLength();
            }
        }
        onProgress(runnable, getTaskPercent());
    }

    public int getTaskPercent() {
        return progress * 100 / maxProgress;
    }

    public String getProgress() {
        return progress + "-" + maxProgress;
    }

    public void addCallback(TaskRunningCallback callback) {
        callbacks.add(callback);
    }

    public void addCallbacks(List<TaskRunningCallback> callbacks) {
        this.callbacks.addAll(callbacks);
    }

    @Override
    public void onStart(TaskRunnable runnable) {
        for (TaskRunningCallback callback : callbacks) {
            if (callback != null) callback.onStart(runnable);
        }
    }

    @Override
    public void onEnd(TaskRunnable runnable, boolean succeed) {
        for (TaskRunningCallback callback : callbacks) {
            if (callback != null) callback.onEnd(runnable, succeed);
        }
    }

    @Override
    public void onProgress(TaskRunnable runnable, int percent) {
        for (TaskRunningCallback callback : callbacks) {
            if (callback != null) callback.onProgress(runnable, percent);
        }
    }
}
