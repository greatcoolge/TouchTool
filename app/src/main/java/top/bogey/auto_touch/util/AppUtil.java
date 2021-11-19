package top.bogey.auto_touch.util;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.graphics.drawable.Drawable;
import android.provider.Settings;
import android.text.TextUtils;

import androidx.annotation.ColorInt;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.R;

public class AppUtil {


    public static boolean isAccessibilityServiceOn(Context context){
        String serviceName = context.getPackageName() + "/" + MainAccessibilityService.class.getCanonicalName();
        boolean isEnabled = false;
        try {
            isEnabled = 1 == Settings.Secure.getInt(context.getApplicationContext().getContentResolver(), Settings.Secure.ACCESSIBILITY_ENABLED);
        } catch (Settings.SettingNotFoundException e) {
            e.printStackTrace();
        }
        if (isEnabled){
            String s = Settings.Secure.getString(context.getApplicationContext().getContentResolver(), Settings.Secure.ENABLED_ACCESSIBILITY_SERVICES);
            if (s != null){
                TextUtils.SimpleStringSplitter splitter = new TextUtils.SimpleStringSplitter(':');
                splitter.setString(s);
                while (splitter.hasNext()){
                    String next = splitter.next();
                    if (serviceName.equalsIgnoreCase(next)){
                        return true;
                    }
                }
            }
        }
        return false;
    }

    public static void showSimpleDialog(Activity activity, int msg, SelectCallback callback){
        new AlertDialog.Builder(activity).setTitle(R.string.dialog_title).setMessage(msg)
                .setPositiveButton(R.string.enter, (dialog, which) -> {
                    dialog.dismiss();
                    if (callback != null){
                        callback.onEnter();
                    }
                })
                .setNegativeButton(R.string.cancel, (dialog, which) ->{
                    dialog.dismiss();
                    if (callback != null) {
                        callback.onCancel();
                    }
                })
                .create().show();
    }

    public static void showSimpleDialog(Activity activity, String msg, SelectCallback callback){
        new AlertDialog.Builder(activity).setTitle(R.string.dialog_title).setMessage(msg)
                .setPositiveButton(R.string.enter, (dialog, which) -> {
                    dialog.dismiss();
                    if (callback != null){
                        callback.onEnter();
                    }
                })
                .setNegativeButton(R.string.cancel, (dialog, which) ->{
                    dialog.dismiss();
                    if (callback != null) {
                        callback.onCancel();
                    }
                })
                .create().show();
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    public static Drawable getDrawable(Context context, ApplicationInfo info){
        if (info == null){
            return context.getDrawable(R.mipmap.ic_common);
        }
        PackageManager manager = context.getPackageManager();
        return info.loadIcon(manager);
    }

    @ColorInt
    public static int getGroupColor(Context context, int group){
        int[] colors = {R.color.amber_500, R.color.red_500, R.color.blue_500, R.color.green_500};
        return context.getResources().getColor(colors[group], null);
    }
}
