package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Rect;
import android.view.MotionEvent;
import android.widget.Toast;

import androidx.annotation.NonNull;

import top.bogey.auto_touch.CaptureService;
import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.util.SelectCallback;

public class ImagePicker extends NodePicker{
    private Bitmap bitmap;
    private CaptureService.CaptureBinder binder;

    public ImagePicker(@NonNull Context context, PickerCallback pickerCallback) {
        super(context, null, pickerCallback);
        layout = new ImagePickerView(context, new SelectCallback() {
            @Override
            public void onEnter() {
                ImagePickerView pickerView = (ImagePickerView) layout;
                startCapture(pickerView.getMarkArea());
                dismiss();
            }

            @Override
            public void onCancel() { }
        });
        floatCallback = new TouchCallback();
    }

    @Override
    public void show(int x, int y) {
        super.show(x, y);
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
            if (service.binder == null){
                Toast.makeText(context, R.string.check_image, Toast.LENGTH_LONG).show();
                service.startCaptureService(true, result -> {
                    if (result){
                        binder = service.binder;
                    }
                });
            } else {
                binder = service.binder;
            }
        }
    }

    public Bitmap getBitmap(){
        return bitmap;
    }

    private void startCapture(Rect rect){
        if (binder != null) bitmap = binder.captureImage(rect);
    }

    private class TouchCallback extends FloatPickerShowCallback {
        @Override
        public void onDismiss() {
            super.onDismiss();
            if (pickerCallback != null){
                pickerCallback.call(ImagePicker.this);
            }
        }

        @Override
        public void onTouch(MotionEvent motionEvent) {
            super.onTouch(motionEvent);
            ImagePickerView pickerView = (ImagePickerView) layout;
            pickerView.onTouch(motionEvent);
        }
    }
}
