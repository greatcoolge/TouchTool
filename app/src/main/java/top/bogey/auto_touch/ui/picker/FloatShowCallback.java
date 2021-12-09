package top.bogey.auto_touch.ui.picker;

import android.app.ActivityManager;
import android.content.Context;
import android.view.View;

import androidx.annotation.Nullable;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;

public class FloatShowCallback extends FloatCallback{
    @Override
    public void createdResult(boolean b, @Nullable String s, @Nullable View view) {
        super.createdResult(b, s, view);
        if (b){
            MainActivity activity = MainApplication.getActivity();
            if (activity != null){
                activity.moveTaskToBack(true);
            }
        }
    }

    @Override
    public void dismiss() {
        super.dismiss();
        MainActivity activity = MainApplication.getActivity();
        if (activity != null){
            ActivityManager manager = (ActivityManager) activity.getSystemService(Context.ACTIVITY_SERVICE);
            manager.moveTaskToFront(activity.getTaskId(), ActivityManager.MOVE_TASK_WITH_HOME);
        }
    }
}
