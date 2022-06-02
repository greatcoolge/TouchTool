package top.bogey.auto_touch.utils;

public interface TaskCallback {
    void onStart();
    void onEnd(boolean succeed);
    void onProgress(int percent);
}
