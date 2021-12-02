package top.bogey.auto_touch.util;

import android.content.Intent;

public interface PermissionResultCallback {
    void onResult(int code, Intent data);
}
