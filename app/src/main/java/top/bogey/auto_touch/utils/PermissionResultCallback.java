package top.bogey.auto_touch.utils;

import android.content.Intent;

public interface PermissionResultCallback {
    void onResult(int code, Intent intent);
}
