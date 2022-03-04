package top.bogey.auto_touch.util;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.graphics.Point;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Build;
import android.provider.Settings;
import android.text.TextUtils;
import android.view.WindowManager;

import androidx.annotation.ColorInt;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.room.bean.Pos;

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

    public static String getIdentityCode(Object obj){
        return obj.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(obj));
    }

    public static int dp2px(Context context, int dp){
        return Math.round(dp * context.getResources().getDisplayMetrics().density);
    }

    public static Point getScreenSize(Context context){
        WindowManager manager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        Point point = new Point();
        manager.getDefaultDisplay().getRealSize(point);
        return point;
    }

    public static Pos px2percent(Context context, Pos pos){
        Point size = getScreenSize(context);
        return new Pos(Math.round(pos.getX() * 100f / size.x), Math.round(pos.getY() * 100f / size.y));
    }

    public static Pos percent2px(Context context, Pos pos){
        Point size = getScreenSize(context);
        return new Pos(Math.round(size.x * pos.getX() / 100f), Math.round(size.y * pos.getY() / 100f));
    }

    public static void gotoAutostartSettingIntent(Activity activity){
        String device = Build.MANUFACTURER;
        List<String> list = PackagesInfo.getInstance().get(device.toLowerCase());
        Intent intent = new Intent();
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        if (list != null){
            ComponentName name = new ComponentName(list.get(0), list.get(1));
            intent.setComponent(name);
        } else {
            intent.setAction("android.settings.APPLICATION_DETAILS_SETTINGS");
            intent.setData(Uri.fromParts("package", activity.getPackageName(), null));
        }
        try{
            activity.startActivity(intent);
        }catch (Exception ignored){}
    }

    public static void openApp(Context context, String pkgName){
        Intent intent = context.getPackageManager().getLaunchIntentForPackage(pkgName);
        if (intent != null){
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED);
            context.startActivity(intent);
        }
    }

    private static class PackagesInfo extends HashMap<String, List<String>> {
        private static PackagesInfo packagesInfo;

        public PackagesInfo(){
            put("xiaomi", Arrays.asList("com.miui.securitycenter", "com.miui.permcenter.autostart.AutoStartManagementActivity"));
            put("blackshark", Arrays.asList("com.miui.securitycenter", "com.miui.permcenter.autostart.AutoStartManagementActivity"));
            put("samsung", Arrays.asList("com.samsung.android.sm", "com.samsung.android.sm.app.dashboard.SmartManagerDashBoardActivity"));
            put("huawei", Arrays.asList("com.huawei.systemmanager", "com.huawei.systemmanager.appcontrol.activity.StartupAppControlActivity"));
            put("vivo", Arrays.asList("com.iqoo.secure", "com.iqoo.secure.ui.phoneoptimize.AddWhiteListActivity"));
            put("meizu", Arrays.asList("com.meizu.safe", "com.meizu.safe.permission.SmartBGActivity"));
            put("oppo", Arrays.asList("com.coloros.oppoguardelf", "com.coloros.powermanager.fuelgaue.PowerUsageModelActivity"));
            put("oneplus", Arrays.asList("com.oneplus.security", "com.oneplus.security.chainlaunch.view.ChainLaunchAppListActivity"));
            put("360", Arrays.asList("com.yulong.android.coolsafe", "com.yulong.android.coolsafe.ui.activity.autorun.AutoRunListActivity"));
            put("yulong", Arrays.asList("com.yulong.android.coolsafe", "com.yulong.android.coolsafe.ui.activity.autorun.AutoRunListActivity"));

            put("zte", Arrays.asList("com.zte.heartyservice", "com.zte.heartyservice.autorun.AppAutoRunManager"));
            put("smartisan", Arrays.asList("com.smartisanos.security", "com.smartisanos.security.invokeHistory.InvokeHistoryActivity"));
            put("coolpad", Arrays.asList("com.yulong.android.security", "com.yulong.android.seccenter.tabbarmain"));
            put("lenovo", Arrays.asList("com.lenovo.security", "com.lenovo.security.purebackground.PureBackgroundActivity"));
            put("asus", Arrays.asList("com.asus.mobilemanager", "com.asus.mobilemanager.MainActivity"));
        }

        public static PackagesInfo getInstance(){
            if (packagesInfo == null) packagesInfo = new PackagesInfo();
            return packagesInfo;
        }
    }
}
