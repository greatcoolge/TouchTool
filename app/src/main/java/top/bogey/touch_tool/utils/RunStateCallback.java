package top.bogey.touch_tool.utils;

import top.bogey.touch_tool.room.data.TaskCallable;

public interface RunStateCallback {
    void onNewTask(TaskCallable callable);
    void onTaskProgress(TaskCallable callable, int progress);
    void onTaskEnd(TaskCallable callable);
}
