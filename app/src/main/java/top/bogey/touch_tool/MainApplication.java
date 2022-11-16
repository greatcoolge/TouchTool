package top.bogey.touch_tool;

import android.app.Application;

import com.tencent.mmkv.MMKV;

import top.bogey.touch_tool.ui.setting.KeepAliveService;
import top.bogey.touch_tool.ui.setting.SettingSave;

public class MainApplication extends Application {
    private static MainActivity activity;
    private static MainAccessibilityService service;

    private static KeepAliveService aliveService;

    @Override
    public void onCreate() {
        super.onCreate();
        MMKV.initialize(this);
        SettingSave.getInstance().setDynamicColor(this, SettingSave.getInstance().isDynamicColor());
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
}
