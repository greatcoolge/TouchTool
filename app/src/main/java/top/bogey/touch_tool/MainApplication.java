package top.bogey.touch_tool;

import android.app.Application;

import androidx.annotation.NonNull;

import com.tencent.mmkv.MMKV;

import java.util.Arrays;

import top.bogey.touch_tool.ui.setting.KeepAliveService;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.LogUtils;
import top.bogey.touch_tool.ui.setting.SettingSave;

public class MainApplication extends Application implements Thread.UncaughtExceptionHandler {
    private static MainActivity activity;
    private static MainAccessibilityService service;

    private static KeepAliveService aliveService;

    @Override
    public void onCreate() {
        super.onCreate();
        MMKV.initialize(this);
        SettingSave.getInstance().setDynamicColor(this, SettingSave.getInstance().isDynamicColor());
        Thread.setDefaultUncaughtExceptionHandler(this);
    }

    public static MainActivity getActivity() {
        return activity;
    }

    public static void setActivity(MainActivity activity) {
        MainApplication.activity = activity;
    }

    public static MainAccessibilityService getService() {
        return service;
    }

    public static void setService(MainAccessibilityService service) {
        MainApplication.service = service;
    }

    public static KeepAliveService getAliveService() {
        return aliveService;
    }

    public static void setAliveService(KeepAliveService aliveService) {
        MainApplication.aliveService = aliveService;
    }

    @Override
    public void uncaughtException(@NonNull Thread t, @NonNull Throwable e) {
        LogUtils.log(LogLevel.HIGH, e + "\n" + Arrays.toString(e.getStackTrace()));
        e.printStackTrace();
        System.exit(996);
    }
}
