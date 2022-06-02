package top.bogey.auto_touch;

import android.app.Application;

import com.google.android.material.color.DynamicColors;

public class MainApplication extends Application {
    private static MainActivity activity;
    private static MainAccessibilityService service;

    @Override
    public void onCreate() {
        super.onCreate();
        DynamicColors.applyToActivitiesIfAvailable(this);
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
}
