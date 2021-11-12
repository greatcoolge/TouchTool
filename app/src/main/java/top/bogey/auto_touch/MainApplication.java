package top.bogey.auto_touch;

import android.app.Application;

public class MainApplication extends Application {
    private static MainActivity activity;

    public static MainActivity getActivity() {
        return activity;
    }

    public static void setActivity(MainActivity activity) {
        MainApplication.activity = activity;
    }
}
