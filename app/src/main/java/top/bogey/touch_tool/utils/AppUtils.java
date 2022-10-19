package top.bogey.touch_tool.utils;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Point;
import android.net.Uri;
import android.os.PowerManager;
import android.provider.Settings;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.tencent.mmkv.MMKV;

import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
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

    public static Point getFixedPosition(int x, int y){
        int offset = MMKV.defaultMMKV().decodeInt(ACTION_TOUCH_OFFSET, 0);
        return new Point((int) (Math.random() * offset * 2 + x - offset), (int) (Math.random() * offset * 2 + y - offset));
    }

    public static void wakeScreen(Context context){
        PowerManager powerManager = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
        PowerManager.WakeLock wakeLock = powerManager.newWakeLock(PowerManager.ACQUIRE_CAUSES_WAKEUP | PowerManager.FULL_WAKE_LOCK, context.getString(R.string.common_package_name));
        wakeLock.acquire(100);
        wakeLock.release();
    }

    @SuppressLint("SimpleDateFormat")
    public static String formatDateMinute(long dateTime){
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");
        Date date = new Date(dateTime);
        return dateFormat.format(date);
    }

    @SuppressLint("SimpleDateFormat")
    public static String formatDateSecond(long dateTime){
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date date = new Date(dateTime);
        return dateFormat.format(date);
    }

    public static long mergeDateTime(long date, long time){
        Calendar baseCalendar = Calendar.getInstance();
        baseCalendar.setTimeInMillis(time);
        Calendar dateCalendar = Calendar.getInstance();
        dateCalendar.setTimeInMillis(date);
        Calendar calendar = Calendar.getInstance();
        calendar.set(dateCalendar.get(Calendar.YEAR), dateCalendar.get(Calendar.MONTH), dateCalendar.get(Calendar.DATE), baseCalendar.get(Calendar.HOUR_OF_DAY), baseCalendar.get(Calendar.MINUTE), 0);
        return calendar.getTimeInMillis();
    }
}
