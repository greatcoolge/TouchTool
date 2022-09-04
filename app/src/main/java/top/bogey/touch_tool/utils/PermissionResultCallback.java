package top.bogey.touch_tool.utils;

import android.content.Intent;

public interface PermissionResultCallback {
    void onResult(int code, Intent intent);
}
