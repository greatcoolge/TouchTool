package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.PorterDuff;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.graphics.RectF;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;
import android.widget.Toast;

import top.bogey.auto_touch.CaptureService;
import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentPickerImageBinding;
import top.bogey.auto_touch.util.AppUtil;

@SuppressLint("ViewConstructor")
public class ImagePicker extends NodePicker {
    private enum AdjustMode {NONE, DRAG, TOP_LEFT, BOTTOM_RIGHT}

    private CaptureService.CaptureBinder binder;
    private Bitmap bitmap;

    private final FloatFragmentPickerImageBinding binding;

    private final Paint markPaint;
    public RectF markArea = new RectF();

    private AdjustMode adjustMode = AdjustMode.NONE;
    private boolean isMarked = false;

    private float lastX = 0;
    private float lastY = 0;

    private final int offset;

    public ImagePicker(Context context, PickerCallback pickerCallback) {
        super(context, pickerCallback);
        floatCallback = new TouchCallback();
        binding = FloatFragmentPickerImageBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());

        binding.saveButton.setOnClickListener(v -> {
            pickerCallback.call(this);
            dismiss();
        });

        binding.closeButton.setOnClickListener(v -> {
            isMarked = false;
            markArea = new RectF();
            refreshUI();
        });

        markPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        markPaint.setStyle(Paint.Style.FILL);
        markPaint.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.CLEAR));

        offset = AppUtil.dp2px(context, 4);
    }

    private Rect getMarkArea(){
        int[] locations = new int[2];
        getLocationOnScreen(locations);
        markArea.top += locations[1];
        markArea.bottom += locations[1];
        return new Rect((int) markArea.left, (int)markArea.top, (int) markArea.right, (int) markArea.bottom);
    }

    public Bitmap getBitmap(){
        if (binder != null) bitmap = binder.captureImage(getMarkArea());
        return bitmap;
    }

    @Override
    public void show() {
        super.show();
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
            if (service.binder == null){
                Toast.makeText(getContext(), R.string.check_image, Toast.LENGTH_LONG).show();
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

    @Override
    public void dispatchDraw(Canvas canvas) {
        super.dispatchDraw(canvas);
        canvas.drawRect(markArea, markPaint);
    }

    public void onTouch(MotionEvent event) {
        float rawX = event.getRawX();
        float rawY = event.getRawY();
        float x = event.getX();
        float y = event.getY();
        int[] location = new int[2];

        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                lastX = x;
                lastY = y;
                if (isMarked) {
                    binding.moveRight.getLocationOnScreen(location);
                    Rect rect = new Rect(location[0], location[1], location[0] + binding.moveRight.getWidth(), location[1] + binding.moveRight.getHeight());
                    if (rect.contains((int) rawX, (int) rawY)) {
                        adjustMode = AdjustMode.BOTTOM_RIGHT;
                    } else {
                        location = new int[2];
                        binding.moveLeft.getLocationOnScreen(location);
                        rect = new Rect(location[0], location[1], location[0] + binding.moveLeft.getWidth(), location[1] + binding.moveLeft.getHeight());
                        if (rect.contains((int) rawX, (int) rawY)) {
                            adjustMode = AdjustMode.TOP_LEFT;
                        } else {
                            location = new int[2];
                            binding.markBox.getLocationOnScreen(location);
                            rect = new Rect(location[0], location[1], location[0] + binding.markBox.getWidth(), location[1] + binding.markBox.getHeight());
                            if (rect.contains((int) rawX, (int) rawY)) {
                                adjustMode = AdjustMode.DRAG;
                            }
                        }
                    }
                } else {
                    adjustMode = AdjustMode.NONE;
                    markArea = new RectF(x, y, x, y);
                }
                break;
            case MotionEvent.ACTION_MOVE:
                if (isMarked){
                    float dx = x - lastX;
                    float dy = y - lastY;
                    switch (adjustMode){
                        case DRAG:
                            markArea.left = Math.max(0, markArea.left + dx);
                            markArea.top = Math.max(0, markArea.top + dy);
                            markArea.right = Math.max(0, markArea.right + dx);
                            markArea.bottom = Math.max(0, markArea.bottom + dy);
                            break;
                        case TOP_LEFT:
                            markArea.left = Math.max(0, markArea.left + dx);
                            markArea.top = Math.max(0, markArea.top + dy);
                            break;
                        case BOTTOM_RIGHT:
                            markArea.right = Math.max(0, markArea.right + dx);
                            markArea.bottom = Math.max(0, markArea.bottom + dy);
                            break;
                    }
                    markArea.sort();
                    lastX = x;
                    lastY = y;
                } else {
                    markArea.right = x;
                    markArea.bottom = y;
                    markArea.sort();
                }
                break;

            case MotionEvent.ACTION_UP:
                if (isMarked){
                    adjustMode = AdjustMode.NONE;
                } else {
                    if (!(markArea.width() == 0 || markArea.height() == 0)){
                        markArea.right = x;
                        markArea.bottom = y;
                        markArea.sort();
                        isMarked = true;
                    }
                }
                break;
        }
        refreshUI();
    }

    private void refreshUI(){
        binding.markBox.setVisibility(isMarked ? VISIBLE : INVISIBLE);
        binding.buttonBox.setVisibility(isMarked ? VISIBLE : INVISIBLE);
        ViewGroup.LayoutParams params = binding.markBox.getLayoutParams();
        params.width = (int) markArea.width() + 2 * offset;
        params.height = (int) markArea.height() + 2 * offset;
        binding.markBox.setLayoutParams(params);

        binding.markBox.setX(markArea.left - offset);
        binding.markBox.setY(markArea.top - offset);

        binding.buttonBox.setX(markArea.left + (markArea.width() - binding.buttonBox.getWidth()) / 2);
        if (markArea.bottom + offset * 2 + binding.buttonBox.getHeight() > binding.getRoot().getHeight()){
            binding.buttonBox.setY(markArea.top - offset * 2 - binding.buttonBox.getHeight());
        } else {
            binding.buttonBox.setY(markArea.bottom + offset * 2);
        }
        postInvalidate();
    }

    private class TouchCallback extends FloatShowPickerCallback{
        @Override
        public void onTouch(MotionEvent motionEvent) {
            super.onTouch(motionEvent);
            ImagePicker.this.onTouch(motionEvent);
        }
    }

}
