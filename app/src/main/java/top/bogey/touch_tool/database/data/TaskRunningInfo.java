package top.bogey.touch_tool.database.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

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

    private final Map<Task, Boolean> runningState = new HashMap<>();
    private int progress = 0;

    public TaskRunningInfo(TaskRunnable runnable, String pkgName, int maxProgress) {
        this.runnable = runnable;
        this.pkgName = pkgName;
        this.maxProgress = maxProgress;
    }

    public String getPkgName() {
        return pkgName;
    }

    public Boolean hasRunning(Task task) {
        return runningState.get(task);
    }

    public boolean isRunning(Task task) {
        return Boolean.TRUE.equals(hasRunning(task));
    }

    public void setRunning(Task task, boolean running) {
        Boolean hasRunning = hasRunning(task);
        if (hasRunning == null || Boolean.TRUE.equals(hasRunning)) {
            runningState.put(task, running);
        }
    }

    public void addProgress(Task baseTask, Action action, boolean skip) {
        if (action.getType() == ActionType.TASK && skip) {
            Task task = baseTask.getSubTaskById(((TaskAction) action).getId());
            if (task != null) {
                progress += task.getLength();
            }
        }
        progress++;

        onProgress(runnable, getTaskPercent());
    }

    public void jumpProgress(int start, Task baseTask, List<Action> actions) {
        progress = start;
        for (Action action : actions) {
            addProgress(baseTask, action, true);
        }
    }

    public int getTaskPercent() {
        return progress * 100 / maxProgress;
    }

    public String getProgressString() {
        return progress + "/" + maxProgress;
    }

    public int getProgress() {
        return progress;
    }

    public void addCallback(TaskRunningCallback callback) {
        callbacks.add(callback);
    }

    public void addCallbacks(List<TaskRunningCallback> callbacks) {
        this.callbacks.addAll(callbacks);
    }

    @Override
    public void onStart(TaskRunnable runnable) {
        callbacks.stream().filter(Objects::nonNull).forEach(callback -> callback.onStart(runnable));
    }

    @Override
    public void onEnd(TaskRunnable runnable, boolean succeed) {
        callbacks.stream().filter(Objects::nonNull).forEach(callback -> callback.onEnd(runnable, succeed));
    }

    @Override
    public void onProgress(TaskRunnable runnable, int percent) {
        callbacks.stream().filter(Objects::nonNull).forEach(callback -> callback.onProgress(runnable, percent));
    }
}
