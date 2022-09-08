package top.bogey.touch_tool;

import android.app.Application;
import android.content.Context;
import android.content.SharedPreferences;

import androidx.appcompat.app.AppCompatDelegate;

import com.google.android.material.color.DynamicColors;

public class MainApplication extends Application {
    public static final String NIGHT_MODE = "night_mode";

    private static MainActivity activity;
    private static MainAccessibilityService service;

    @Override
    public void onCreate() {
        super.onCreate();
        DynamicColors.applyToActivitiesIfAvailable(this);
        initNightMode(this);
    }

    public static void initNightMode(Context context){
        SharedPreferences preferences = context.getSharedPreferences(MainAccessibilityService.SAVE_PATH, Context.MODE_PRIVATE);
        String nightMode = preferences.getString(NIGHT_MODE, "-1");
        initNightMode(context, Integer.parseInt(nightMode));
    }

    public static void initNightMode(Context context, int nightMode){
        AppCompatDelegate.setDefaultNightMode(nightMode);
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
