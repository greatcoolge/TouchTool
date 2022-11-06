package top.bogey.touch_tool.utils;

import top.bogey.touch_tool.database.bean.Task;

public interface TaskChangedCallback {
    void onChanged(Task task);
    void onRemoved(Task task);
}
