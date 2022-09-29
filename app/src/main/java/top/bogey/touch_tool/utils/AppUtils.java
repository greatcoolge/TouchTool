package top.bogey.touch_tool.utils;

import static top.bogey.touch_tool.MainAccessibilityService.SAVE_PATH;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Bitmap;
import android.graphics.Point;
import android.net.Uri;
import android.provider.Settings;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import java.lang.reflect.Method;
import java.util.List;

import top.bogey.touch_tool.R;

public class AppUtils {
    public static final String ACTION_TOUCH_OFFSET = "action_touch_offset";

    public static native MatchResult nativeMatchTemplate(Bitmap bitmap, Bitmap temp, int method);
    public static native List<MatchResult> nativeMatchColor(Bitmap bitmap, int[] hsvColor);

    public static void showDialog(Context context, int msg, SelectCallback callback){
        new MaterialAlertDialogBuilder(context)
                .setTitle(R.string.dialog_title)
                .setMessage(msg)
                .setPositiveButton(R.string.enter, (dialog, which) -> {
                    dialog.dismiss();
                    if (callback != null) callback.onEnter();
                })
                .setNegativeButton(R.string.cancel, (dialog, which) -> {
                    dialog.dismiss();
                    if (callback != null) callback.onCancel();
                })
                .show();
    }

    public static void gotoAppDetailSetting(Activity activity){
        Intent intent = new Intent();
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.setAction("android.settings.APPLICATION_DETAILS_SETTINGS");
        intent.setData(Uri.fromParts("package", activity.getPackageName(), null));
        try{
            activity.startActivity(intent);
        }catch (Exception ignored){}
    }

    public static boolean checkFloatPermission(Context context){
        try {
            Method canDrawOverlays = Settings.class.getDeclaredMethod("canDrawOverlays", Context.class);
            return Boolean.TRUE.equals(canDrawOverlays.invoke(null, context)) ;
        } catch (Exception ignored){}
        return false;
    }

    public static String getIdentityCode(Object obj){
        return obj.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(obj));
    }

    public static Point getFixedPosition(Context context, int x, int y){
        SharedPreferences preferences = context.getSharedPreferences(SAVE_PATH, Context.MODE_PRIVATE);
        int offset = preferences.getInt(ACTION_TOUCH_OFFSET, 0);
        return new Point((int) (Math.random() * offset * 2 + x - offset), (int) (Math.random() * offset * 2 + y - offset));
    }
}
