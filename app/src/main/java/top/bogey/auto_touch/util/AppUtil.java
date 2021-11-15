package top.bogey.auto_touch.util;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.provider.Settings;
import android.text.TextUtils;

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
                .setPositiveButton(R.string.dialog_enter, (dialog, which) -> {
                    dialog.dismiss();
                    if (callback != null){
                        callback.onEnter();
                    }
                })
                .setNegativeButton(R.string.dialog_cancel, (dialog, which) ->{
                    dialog.dismiss();
                    if (callback != null) {
                        callback.onCancel();
                    }
                })
                .create().show();
    }

}
