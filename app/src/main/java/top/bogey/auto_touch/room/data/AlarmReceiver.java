package top.bogey.auto_touch.room.data;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class AlarmReceiver extends BroadcastReceiver {
    public static final String ACTION = "top.bogey.auto_touch.room.data.AlarmReceiver";

    @Override
    public void onReceive(Context context, Intent intent) {
        String tag = intent.getAction();
        if (tag.equals(ACTION)){
            String id = intent.getStringExtra("id");
            Log.d("TAG", "onReceive: " + id);
        }
    }
}
