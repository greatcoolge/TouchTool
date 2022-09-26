package top.bogey.touch_tool;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.PixelFormat;
import android.graphics.Rect;
import android.hardware.display.DisplayManager;
import android.hardware.display.VirtualDisplay;
import android.media.Image;
import android.media.ImageReader;
import android.media.projection.MediaProjection;
import android.media.projection.MediaProjectionManager;
import android.os.Binder;
import android.os.Build;
import android.os.IBinder;
import android.util.DisplayMetrics;
import android.view.WindowManager;

import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.ui.setting.DebugLevel;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.LogUtils;
import top.bogey.touch_tool.utils.MatchResult;

public class MainCaptureService extends Service {
    public static final String RUNNING_CHANNEL_ID = "RUNNING_CHANNEL_ID";

    private static final String NOTIFICATION_CHANNEL_ID = "NOTIFICATION_CHANNEL_ID";

    public static final int NOTIFICATION_ID = 10000;

    private MediaProjection projection;
    private ImageReader imageReader;
    private VirtualDisplay virtualDisplay;
    private Image image;
    private static final Boolean lock = Boolean.FALSE;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        if (projection == null){
            MediaProjectionManager manager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
            Intent data = intent.getParcelableExtra("Data");
            projection = manager.getMediaProjection(Activity.RESULT_OK, data);
            setVirtualDisplay();
        }
        return new CaptureServiceBinder();
    }

    @Override
    public boolean onUnbind(Intent intent) {
        return super.onUnbind(intent);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        createNotification();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (virtualDisplay != null) virtualDisplay.release();
        if (projection != null) projection.stop();

        NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
        notificationManager.cancel(NOTIFICATION_ID + 1);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent.getBooleanExtra("StopCapture", false)){
            MainAccessibilityService service = MainApplication.getService();
            if (service != null){
                service.stopCaptureService();
            }
        }
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        if (virtualDisplay != null) virtualDisplay.release();
        if (imageReader != null) imageReader.close();
        if (image != null) {
            synchronized (lock) {
                image.close();
                image = null;
            }
        }
        setVirtualDisplay();
    }

    private void createNotification(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O){
            NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);

            NotificationChannel runningChannel = notificationManager.getNotificationChannel(RUNNING_CHANNEL_ID);
            if (runningChannel == null){
                runningChannel = new NotificationChannel(RUNNING_CHANNEL_ID, getString(R.string.capture_service_running_channel_name), NotificationManager.IMPORTANCE_DEFAULT);
                runningChannel.setDescription(getString(R.string.capture_service_running_channel_tips));
                notificationManager.createNotificationChannel(runningChannel);
            }

            NotificationChannel channel = new NotificationChannel(NOTIFICATION_CHANNEL_ID, getString(R.string.capture_service_notification_channel_name), NotificationManager.IMPORTANCE_DEFAULT);
            channel.setDescription(getString(R.string.capture_service_running_channel_tips));
            notificationManager.createNotificationChannel(channel);

            Notification foregroundNotification = new NotificationCompat.Builder(this, RUNNING_CHANNEL_ID).build();
            startForeground(NOTIFICATION_ID, foregroundNotification);

            Intent intent = new Intent(this, MainCaptureService.class);
            intent.putExtra("StopCapture", true);
            PendingIntent pendingIntent = PendingIntent.getService(this, 0, intent, PendingIntent.FLAG_IMMUTABLE);

            Notification closeNotification = new NotificationCompat.Builder(this, NOTIFICATION_CHANNEL_ID)
                    .setAutoCancel(true)
                    .setSmallIcon(R.mipmap.ic_launcher)
                    .setContentTitle(getString(R.string.capture_service_notification_title))
                    .setContentText(getString(R.string.capture_service_notification_text))
                    .setContentIntent(pendingIntent)
                    .setOngoing(true)
                    .build();

            closeNotification.flags |= Notification.FLAG_NO_CLEAR;
            notificationManager.notify(NOTIFICATION_ID + 1, closeNotification);
        }
    }

    @SuppressLint("WrongConstant")
    private void setVirtualDisplay(){
        WindowManager manager = (WindowManager) getSystemService(Context.WINDOW_SERVICE);
        DisplayMetrics metrics = new DisplayMetrics();
        manager.getDefaultDisplay().getRealMetrics(metrics);
        imageReader = ImageReader.newInstance(metrics.widthPixels, metrics.heightPixels, PixelFormat.RGBA_8888, 2);
        imageReader.setOnImageAvailableListener(reader -> {
            synchronized (lock){
                if (image != null) image.close();
                image = reader.acquireLatestImage();
            }
        }, null);
        virtualDisplay = projection.createVirtualDisplay("CaptureService", metrics.widthPixels, metrics.heightPixels, metrics.densityDpi, DisplayManager.VIRTUAL_DISPLAY_FLAG_AUTO_MIRROR, imageReader.getSurface(), null, null);
    }

    public class CaptureServiceBinder extends Binder{

        public List<Rect> matchColor(Bitmap bitmap, int[] color){
            List<MatchResult> matchResults = AppUtils.nativeMatchColor(bitmap, color);
            if (matchResults != null){
                matchResults.sort((o1, o2) -> o2.value - o1.value);
                List<Rect> rectList = new ArrayList<>();
                for (MatchResult matchResult : matchResults) {
                    rectList.add(matchResult.rect);
                }
                return rectList;
            }
            return null;
        }

        public List<Rect> matchColor(int[] color){
            Bitmap bitmap = getCurrImage();
            List<Rect> rectList = matchColor(bitmap, color);
            bitmap.recycle();
            return rectList;
        }

        public Rect matchImage(Bitmap sourceBitmap, Bitmap matchBitmap, int matchValue){
            if (sourceBitmap == null || matchBitmap == null) return null;
            MatchResult matchResult = AppUtils.nativeMatchTemplate(sourceBitmap, matchBitmap, 5);
            LogUtils.log(MainCaptureService.this, DebugLevel.MIDDLE, getString(R.string.log_match_image), 0, getString(R.string.log_match_image_value, matchValue, matchResult.value));
            if (Math.min(100, matchValue) > matchResult.value) return null;
            return matchResult.rect;
        }

        public Rect matchImage(Bitmap matchBitmap, int matchValue){
            Bitmap bitmap = getCurrImage();
            Rect rect = matchImage(bitmap, matchBitmap, matchValue);
            bitmap.recycle();
            return rect;
        }

        public Bitmap getCurrImage(){
            Image currImage = image;
            if (currImage == null) return null;

            synchronized (lock){
                Image.Plane[] planes = currImage.getPlanes();
                ByteBuffer buffer = planes[0].getBuffer();
                int pixelStride = planes[0].getPixelStride();
                int rowStride = planes[0].getRowStride();
                int width = currImage.getWidth();
                int height = currImage.getHeight();
                Bitmap bitmap = Bitmap.createBitmap(width + (rowStride - pixelStride * width) / pixelStride, height, Bitmap.Config.ARGB_8888);
                bitmap.copyPixelsFromBuffer(buffer);
                return bitmap;
            }
        }
    }
}
