package top.bogey.auto_touch.ui.picker;

import android.app.ActivityManager;
import android.content.Context;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;

public class FloatShowCallback extends FloatCallbackImpl {
    @Override
    public void onCreate(boolean succeed) {
        if (succeed){
            MainActivity activity = MainApplication.getActivity();
            if (activity != null){
                activity.moveTaskToBack(true);
            }
        }
    }

    @Override
    public void onDismiss() {
        MainActivity activity = MainApplication.getActivity();
        if (activity != null){
            ActivityManager manager = (ActivityManager) activity.getSystemService(Context.ACTIVITY_SERVICE);
            manager.moveTaskToFront(activity.getTaskId(), ActivityManager.MOVE_TASK_WITH_HOME);
        }
    }
}
