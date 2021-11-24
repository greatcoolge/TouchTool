package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.PixelFormat;
import android.graphics.Rect;
import android.hardware.display.DisplayManager;
import android.hardware.display.VirtualDisplay;
import android.media.Image;
import android.media.ImageReader;
import android.media.projection.MediaProjection;
import android.media.projection.MediaProjectionManager;
import android.os.Handler;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;

import androidx.annotation.NonNull;

import com.lzf.easyfloat.EasyFloat;
import com.lzf.easyfloat.enums.ShowPattern;

import java.nio.ByteBuffer;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class ImagePicker extends NodePicker{
    private Intent intent;

    private ImageReader imageReader;
    private VirtualDisplay virtualDisplay;
    private MediaProjection projection;

    private Bitmap bitmap;

    public ImagePicker(@NonNull Context context, PickerCallback pickerCallback) {
        super(context, null, pickerCallback);
        layout = new ImagePickerView(context, new SelectCallback() {
            @Override
            public void onEnter() {
                ImagePickerView pickerView = (ImagePickerView) layout;
                startCapture(pickerView.markArea);
            }

            @Override
            public void onCancel() { }
        });
        floatCallback = new TouchCallback();
    }

    @Override
    public void show(int gravity, int x, int y) {
        MainActivity activity = MainApplication.getActivity();
        if (activity != null){
            activity.launcherCapture((code, data) -> {
                intent = data;
                EasyFloat.with(activity)
                        .setLayout(layout)
                        .setShowPattern(ShowPattern.ALL_TIME)
                        .setTag(AppUtil.getIdentityCode(this))
                        .setDragEnable(true)
                        .setImmersionStatusBar(false)
                        .setGravity(gravity, x, y)
                        .setMatchParent(true, true)
                        .registerCallbacks(floatCallback)
                        .show();
            });
        }
    }

    public Bitmap getBitmap(){
        return bitmap;
    }

    private void startCapture(Rect rect){
        Handler handler = new Handler();
        handler.postDelayed(this::setVirtualDisplay, 10);
        handler.postDelayed(() -> realCapture(rect), 100);
    }

    @SuppressLint("WrongConstant")
    private void setVirtualDisplay(){
        if (projection == null){
            MediaProjectionManager mediaManager = (MediaProjectionManager) layout.getContext().getSystemService(Context.MEDIA_PROJECTION_SERVICE);
            projection = mediaManager.getMediaProjection(Activity.RESULT_OK, intent);
        }
        WindowManager manager = (WindowManager) layout.getContext().getSystemService(Context.WINDOW_SERVICE);
        DisplayMetrics metrics = new DisplayMetrics();
        manager.getDefaultDisplay().getMetrics(metrics);
        imageReader = ImageReader.newInstance(metrics.widthPixels, metrics.heightPixels, PixelFormat.RGBA_8888, 1);
        virtualDisplay = projection.createVirtualDisplay("CaptureScreen", metrics.widthPixels, metrics.heightPixels, metrics.densityDpi, DisplayManager.VIRTUAL_DISPLAY_FLAG_AUTO_MIRROR, imageReader.getSurface(), null, null);
    }

    private void realCapture(Rect rect){
        Image image = imageReader.acquireLatestImage();
        if (image == null) return;
        Rect realRect = new Rect(rect);
        Rect viewRect = new Rect();
        layout.getWindowVisibleDisplayFrame(viewRect);
        int statusBarHeight = viewRect.top;
        realRect.offset(0, statusBarHeight);

        Image.Plane[] planes = image.getPlanes();
        ByteBuffer buffer = planes[0].getBuffer();
        int pixelStride = planes[0].getPixelStride();
        int rowStride = planes[0].getRowStride();
        try {
            bitmap = Bitmap.createBitmap(image.getWidth() + (rowStride - pixelStride * image.getWidth()) / pixelStride, image.getHeight(), Bitmap.Config.ARGB_8888);
            bitmap.copyPixelsFromBuffer(buffer);
            bitmap = Bitmap.createBitmap(bitmap, realRect.left, realRect.top, realRect.width(), realRect.height());
        } catch (Exception ignored){}
        image.close();
        pickerCallback.call(this);
    }

    private class TouchCallback extends FloatCallback{
        @Override
        public void drag(@NonNull View view, @NonNull MotionEvent motionEvent) {
            super.drag(view, motionEvent);
            ImagePickerView pickerView = (ImagePickerView) layout;
            pickerView.onDrag(motionEvent);
        }

        @Override
        public void dragEnd(@NonNull View view) {
            super.dragEnd(view);
            ImagePickerView pickerView = (ImagePickerView) layout;
            pickerView.onDragEnd();
        }

        @Override
        public void dismiss() {
            super.dismiss();
            if (virtualDisplay != null) {
                virtualDisplay.release();
                virtualDisplay = null;
            }
            if (projection != null) projection.stop();
        }
    }

}
