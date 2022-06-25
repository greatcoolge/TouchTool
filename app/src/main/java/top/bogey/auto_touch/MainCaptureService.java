package top.bogey.auto_touch;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
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
import android.util.Log;
import android.view.WindowManager;

import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.utils.AppUtils;
import top.bogey.auto_touch.utils.MatchResult;

public class MainCaptureService extends Service {
    private static final String NOTIFICATION_CHANNEL_ID = "NOTIFICATION_CHANNEL_ID";
    private static final String NOTIFICATION_CHANNEL_NAME = "NOTIFICATION_CHANNEL_NAME";
    private static final String NOTIFICATION_CHANNEL_DES = "NOTIFICATION_CHANNEL_DES";
    private static final int NOTIFICATION_ID = 10000;

    private MediaProjection projection;
    private VirtualDisplay virtualDisplay;
    private ImageReader imageReader;

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
    }

    private void createNotification(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O){
            Intent intent = new Intent(this, MainCaptureService.class);
            PendingIntent pendingIntent = PendingIntent.getActivity(this, 0, intent, PendingIntent.FLAG_IMMUTABLE);
            Notification notification = new NotificationCompat.Builder(this, NOTIFICATION_CHANNEL_ID)
                    .setLargeIcon(BitmapFactory.decodeResource(getResources(), R.mipmap.ic_launcher))
                    .setContentIntent(pendingIntent)
                    .build();
            NotificationChannel channel = new NotificationChannel(NOTIFICATION_CHANNEL_ID, NOTIFICATION_CHANNEL_NAME, NotificationManager.IMPORTANCE_DEFAULT);
            channel.setDescription(NOTIFICATION_CHANNEL_DES);
            NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
            notificationManager.createNotificationChannel(channel);
            startForeground(NOTIFICATION_ID, notification);
        }
    }

    @SuppressLint("WrongConstant")
    private void setVirtualDisplay(){
        WindowManager manager = (WindowManager) getSystemService(Context.WINDOW_SERVICE);
        DisplayMetrics metrics = new DisplayMetrics();
        manager.getDefaultDisplay().getRealMetrics(metrics);
        imageReader = ImageReader.newInstance(metrics.widthPixels, metrics.heightPixels, PixelFormat.RGBA_8888, 2);
        virtualDisplay = projection.createVirtualDisplay("CaptureService", metrics.widthPixels, metrics.heightPixels, metrics.densityDpi, DisplayManager.VIRTUAL_DISPLAY_FLAG_AUTO_MIRROR, imageReader.getSurface(), null, null);
    }

    public class CaptureServiceBinder extends Binder{

        public List<Rect> matchColor(Bitmap bitmap, int[] color){
            List<MatchResult> matchResults = AppUtils.nativeMatchColor(bitmap, color);
            matchResults.sort((o1, o2) -> o2.value - o1.value);
            List<Rect> rectList = new ArrayList<>();
            for (MatchResult matchResult : matchResults) {
                rectList.add(matchResult.rect);
            }
            return rectList;
        }

        public List<Rect> matchColor(int[] color){
            Bitmap bitmap = getCurrImage();
            List<Rect> rectList = matchColor(bitmap, color);
            bitmap.recycle();
            return rectList;
        }

        public Rect matchImage(Bitmap sourceBitmap, Bitmap matchBitmap, int matchValue){
            if (matchBitmap == null) return null;
            MatchResult matchResult = AppUtils.nativeMatchTemplate(sourceBitmap, matchBitmap, 5);
            Log.d("MatchImage", "" + matchResult.value);
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
            Image currImage = imageReader.acquireLatestImage();
            if (currImage == null) return null;
            Image.Plane[] planes = currImage.getPlanes();
            ByteBuffer buffer = planes[0].getBuffer();
            int pixelStride = planes[0].getPixelStride();
            int rowStride = planes[0].getRowStride();
            try {
                Bitmap bitmap = Bitmap.createBitmap(currImage.getWidth() + (rowStride - pixelStride * currImage.getWidth()) / pixelStride, currImage.getHeight(), Bitmap.Config.ARGB_8888);
                bitmap.copyPixelsFromBuffer(buffer);
                return bitmap;
            } catch (Exception ignored){
                return null;
            } finally {
                currImage.close();
            }
        }
    }
}
