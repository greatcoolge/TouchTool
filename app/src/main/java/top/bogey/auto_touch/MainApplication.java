package top.bogey.auto_touch;

import android.app.Application;

public class MainApplication extends Application {
    private static MainActivity activity;
    private static MainAccessibilityService service;

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
}
