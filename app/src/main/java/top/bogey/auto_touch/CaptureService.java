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

import top.bogey.auto_touch.util.MatchResult;
import top.bogey.auto_touch.util.NativeUtil;

public class CaptureService extends Service {
    private static final String NOTIFICATION_CHANNEL_ID = "CaptureService_channel";
    private static final String NOTIFICATION_CHANNEL_NAME = "CaptureService";
    private static final String NOTIFICATION_CHANNEL_DES = "CaptureService";
    private static final int NOTIFICATION_ID = 10000;

    private MediaProjection projection;
    private VirtualDisplay virtualDisplay;
    private ImageReader imageReader;

    private CaptureBinder binder;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        if (projection == null){
            MainActivity activity = MainApplication.getActivity();
            if (activity != null){
                activity.launcherCapture((code, data) -> {
                    MainAccessibilityService service = MainApplication.getService();
                    if (code == Activity.RESULT_OK){
                        MediaProjectionManager manager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
                        projection = manager.getMediaProjection(code, data);
                        setVirtualDisplay();
                        boolean moveBack = intent.getBooleanExtra("MoveBack", false);
                        if (moveBack){
                            activity.moveTaskToBack(true);
                        }
                        service.callCaptureServiceResult(true);
                    } else {
                        service.callCaptureServiceResult(false);
                    }
                });
            }
        }
        return binder;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        createNotification();
        binder = new CaptureBinder();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (virtualDisplay != null){
            virtualDisplay.release();
        }

        if (projection != null){
            projection.stop();
        }
    }

    private void createNotification(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O){
            Intent intent = new Intent(this, CaptureService.class);
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

    public class CaptureBinder extends Binder{

        public Bitmap captureImage(Rect rect){
            Image image = imageReader.acquireLatestImage();
            if (image == null) return null;
            Image.Plane[] planes = image.getPlanes();
            ByteBuffer buffer = planes[0].getBuffer();
            int pixelStride = planes[0].getPixelStride();
            int rowStride = planes[0].getRowStride();
            try {
                Bitmap bitmap = Bitmap.createBitmap(image.getWidth() + (rowStride - pixelStride * image.getWidth()) / pixelStride, image.getHeight(), Bitmap.Config.ARGB_8888);
                bitmap.copyPixelsFromBuffer(buffer);
                Bitmap newBitmap = Bitmap.createBitmap(bitmap, rect.left, rect.top, rect.width(), rect.height());
                bitmap.recycle();
                return newBitmap;
            } catch (Exception ignored){
                return null;}
            finally {
                image.close();
            }
        }

        public Rect matchImage(Bitmap tempBitmap, int matchValue){
            Image image = imageReader.acquireLatestImage();
            if (image == null) return null;
            Image.Plane[] planes = image.getPlanes();
            ByteBuffer buffer = planes[0].getBuffer();
            int pixelStride = planes[0].getPixelStride();
            int rowStride = planes[0].getRowStride();
            try {
                Bitmap bitmap = Bitmap.createBitmap(image.getWidth() + (rowStride - pixelStride * image.getWidth()) / pixelStride, image.getHeight(), Bitmap.Config.ARGB_8888);
                bitmap.copyPixelsFromBuffer(buffer);
                MatchResult matchResult = NativeUtil.nativeMatchTemplate(bitmap, tempBitmap, 5);
                bitmap.recycle();
                Log.d("MatchImage", "" + matchResult.value);
                if (matchValue > matchResult.value) return null;
                return new Rect(matchResult.x, matchResult.y, matchResult.x + tempBitmap.getWidth(), matchResult.y + tempBitmap.getHeight());
            } catch (Exception ignored){
                return null;
            }
            finally {
                image.close();
            }
        }
    }
}
