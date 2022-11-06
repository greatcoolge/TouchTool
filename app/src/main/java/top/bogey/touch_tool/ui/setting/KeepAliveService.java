package top.bogey.touch_tool.ui.setting;

import static top.bogey.touch_tool.MainCaptureService.NOTIFICATION_ID;
import static top.bogey.touch_tool.MainCaptureService.RUNNING_CHANNEL_ID;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.os.IBinder;

import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;

public class KeepAliveService extends Service {
    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        createNotification();
        MainApplication.setAliveService(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        MainApplication.setAliveService(null);
    }

    private void createNotification() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);

            NotificationChannel runningChannel = notificationManager.getNotificationChannel(RUNNING_CHANNEL_ID);
            if (runningChannel == null) {
                runningChannel = new NotificationChannel(RUNNING_CHANNEL_ID, getString(R.string.capture_service_running_channel_name), NotificationManager.IMPORTANCE_DEFAULT);
                runningChannel.setDescription(getString(R.string.capture_service_running_channel_tips));
                notificationManager.createNotificationChannel(runningChannel);
            }

            Notification foregroundNotification = new NotificationCompat.Builder(this, RUNNING_CHANNEL_ID).build();
            startForeground(NOTIFICATION_ID, foregroundNotification);
        }
    }
}
