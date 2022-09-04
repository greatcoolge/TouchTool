package top.bogey.touch_tool.utils;

public interface TaskCallback {
    void onStart();
    void onEnd(boolean succeed);
    void onProgress(int percent);
}
