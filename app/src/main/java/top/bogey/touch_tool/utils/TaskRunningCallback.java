package top.bogey.touch_tool.utils;

import top.bogey.touch_tool.database.data.TaskRunnable;

public interface TaskRunningCallback {
    void onStart(TaskRunnable runnable);

    void onEnd(TaskRunnable runnable, boolean succeed);

    void onProgress(TaskRunnable runnable, int percent);
}
