package top.bogey.auto_touch.util;

import android.graphics.Bitmap;

public class NativeUtil {
    public static native MatchResult matchTemplate(Bitmap bitmap, Bitmap temp, int method);
}
