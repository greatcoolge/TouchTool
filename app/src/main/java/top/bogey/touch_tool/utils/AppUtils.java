package top.bogey.touch_tool.utils;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.Point;
import android.net.Uri;
import android.os.Parcel;
import android.os.Parcelable;
import android.os.PowerManager;
import android.provider.Settings;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.tencent.mmkv.MMKV;

import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.R;

public class AppUtils {
    public static final String ACTION_TOUCH_OFFSET = "action_touch_offset";

    public static native MatchResult nativeMatchTemplate(Bitmap bitmap, Bitmap temp, int method);

    public static native List<MatchResult> nativeMatchColor(Bitmap bitmap, int[] hsvColor);

    public static void showDialog(Context context, int msg, SelectCallback callback) {
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

    public static void gotoAppDetailSetting(Context context) {
        try {
            Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setData(Uri.parse("package:" + context.getPackageName()));
            context.startActivity(intent);
        } catch (Exception ignored) {
        }
    }

    public static void gotoApp(Context context, String pkgName) {
            try {
                PackageManager manager = context.getPackageManager();
                Intent intent = manager.getLaunchIntentForPackage(pkgName);
                if (intent != null) context.startActivity(intent);
            } catch (Exception ignored) {
            }
    }

    public static boolean checkFloatPermission(Context context) {
        try {
            Method canDrawOverlays = Settings.class.getDeclaredMethod("canDrawOverlays", Context.class);
            return Boolean.TRUE.equals(canDrawOverlays.invoke(null, context));
        } catch (Exception ignored) {
        }
        return false;
    }

    public static String getIdentityCode(Object obj) {
        return obj.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(obj));
    }

    public static Point getFixedPosition(int x, int y) {
        int offset = MMKV.defaultMMKV().decodeInt(ACTION_TOUCH_OFFSET, 0);
        return new Point((int) (Math.random() * offset * 2 + x - offset), (int) (Math.random() * offset * 2 + y - offset));
    }

    public static void wakeScreen(Context context) {
        PowerManager powerManager = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
        PowerManager.WakeLock wakeLock = powerManager.newWakeLock(PowerManager.ACQUIRE_CAUSES_WAKEUP | PowerManager.FULL_WAKE_LOCK, context.getString(R.string.common_package_name));
        wakeLock.acquire(100);
        wakeLock.release();
    }

    public static boolean isIgnoredBattery(Context context){
        PowerManager powerManager = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
        return powerManager.isIgnoringBatteryOptimizations(context.getPackageName());
    }

    public static void gotoBatterySetting(Context context){
        try {
            Intent intent = new Intent(Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setData(Uri.parse("package:" + context.getPackageName()));
            context.startActivity(intent);
        } catch (Exception ignored) {
        }
    }
    
    public static <T extends Parcelable> T copy(T input){
        Parcel parcel = null;
        try{
            parcel = Parcel.obtain();
            parcel.writeParcelable(input, 0);
            parcel.setDataPosition(0);
            return parcel.readParcelable(input.getClass().getClassLoader());
        } finally {
            if (parcel != null) parcel.recycle();
        }
    }

    public static <T extends Parcelable> boolean equals(T a, T b){
        Parcel aParcel = null, bParcel = null;
        try{
            aParcel = Parcel.obtain();
            aParcel.writeParcelable(a, 0);
            byte[] aMarshall = aParcel.marshall();
            bParcel = Parcel.obtain();
            bParcel.writeParcelable(b, 0);
            byte[] bMarshall = bParcel.marshall();
            return Arrays.equals(aMarshall, bMarshall);
        } finally {
            if (aParcel != null) aParcel.recycle();
            if (bParcel != null) bParcel.recycle();
        }
    }

    public static String formatDateLocalDate(Context context, long dateTime){
        Calendar timeCalendar = Calendar.getInstance();
        timeCalendar.setTimeInMillis(dateTime);

        Calendar currCalendar = Calendar.getInstance();
        currCalendar.setTimeInMillis(System.currentTimeMillis());

        StringBuilder builder = new StringBuilder();
        if (timeCalendar.get(Calendar.YEAR) != currCalendar.get(Calendar.YEAR)) builder.append(context.getString(R.string.year, timeCalendar.get(Calendar.YEAR)));
        builder.append(context.getString(R.string.month, timeCalendar.get(Calendar.MONTH)));
        builder.append(context.getString(R.string.day, timeCalendar.get(Calendar.DAY_OF_MONTH)));
        return builder.toString();
    }
    public static String formatDateLocalTime(Context context, long dateTime){
        Calendar timeCalendar = Calendar.getInstance();
        timeCalendar.setTimeInMillis(dateTime);

        StringBuilder builder = new StringBuilder();
        builder.append(context.getString(R.string.hour, timeCalendar.get(Calendar.HOUR_OF_DAY)));
        if (timeCalendar.get(Calendar.MINUTE) != 0) builder.append(context.getString(R.string.minute, timeCalendar.get(Calendar.MINUTE)));
        return  builder.toString();
    }

    public static String formatDateLocalMillisecond(Context context, long dateTime){
        Calendar timeCalendar = Calendar.getInstance();
        timeCalendar.setTimeInMillis(dateTime);

        return context.getString(R.string.hour, timeCalendar.get(Calendar.HOUR_OF_DAY)) +
                context.getString(R.string.minute, timeCalendar.get(Calendar.MINUTE)) +
                context.getString(R.string.second, timeCalendar.get(Calendar.SECOND)) +
                context.getString(R.string.millisecond, timeCalendar.get(Calendar.MILLISECOND));
    }

    public static String formatDateLocalDuration(Context context, long dateTime){
        int hours = (int) (dateTime / 1000 / 60 / 60);
        int minute = (int) (dateTime / 1000 / 60 % 60);

        StringBuilder builder = new StringBuilder();
        if (hours != 0) builder.append(context.getString(R.string.hours, hours));
        if (minute != 0) builder.append(context.getString(R.string.minutes, minute));
        return builder.toString();
    }

    public static long mergeDateTime(long date, long time) {
        Calendar baseCalendar = Calendar.getInstance();
        baseCalendar.setTimeInMillis(time);
        Calendar dateCalendar = Calendar.getInstance();
        dateCalendar.setTimeInMillis(date);
        Calendar calendar = Calendar.getInstance();
        calendar.set(dateCalendar.get(Calendar.YEAR), dateCalendar.get(Calendar.MONTH), dateCalendar.get(Calendar.DATE), baseCalendar.get(Calendar.HOUR_OF_DAY), baseCalendar.get(Calendar.MINUTE), 0);
        return calendar.getTimeInMillis();
    }
}
