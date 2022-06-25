package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.PorterDuff;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.widget.SeekBar;
import android.widget.Toast;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.MainCaptureService;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatPickerColorBinding;
import top.bogey.auto_touch.room.bean.node.ColorNode;
import top.bogey.auto_touch.utils.DisplayUtils;
import top.bogey.auto_touch.utils.FloatBaseCallback;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;

@SuppressLint("ViewConstructor")
public class ColorPickerFloatView extends BasePickerFloatView {
    private final ColorNode colorNode;

    private final FloatPickerColorBinding binding;

    private MainCaptureService.CaptureServiceBinder binder;

    private final Paint bitmapPaint;
    private Bitmap showBitmap;

    private final Paint markPaint;
    public List<Rect> markArea = new ArrayList<>();

    private boolean isMarked = false;
    private int[] color = new int[3];
    private int value = 1;

    private float lastX, lastY;
    private boolean drag = false;

    public ColorPickerFloatView(Context context, PickerCallback pickerCallback, ColorNode colorNode) {
        super(context, pickerCallback);
        this.colorNode = colorNode;

        floatCallback = new ImagePickerCallback();

        binding = FloatPickerColorBinding.inflate(LayoutInflater.from(context), this, true);

        binding.saveButton.setOnClickListener(v -> {
            pickerCallback.onComplete(this);
            dismiss();
        });

        binding.closeButton.setOnClickListener(v -> {
            isMarked = false;
            markArea.clear();
            refreshUI();
        });

        binding.closeButton.setOnLongClickListener(v -> {
            dismiss();
            return true;
        });

        binding.seekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                value = Math.max(1, progress);
                refreshUI();
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {

            }
        });

        markPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        markPaint.setStyle(Paint.Style.FILL);
        markPaint.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.DST_OUT));

        bitmapPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        bitmapPaint.setFilterBitmap(true);
        bitmapPaint.setDither(true);
    }

    public ColorNode.ColorInfo getColor(){
        Rect rect = markArea.get(value);
        return new ColorNode.ColorInfo(color, rect.width() * rect.height());
    }

    public void realShow(int delay){
        postDelayed(() -> {
            EasyFloat.show(tag);
            if (binder != null){
                Bitmap bitmap = binder.getCurrImage();
                if (bitmap != null) {
                    int[] location = new int[2];
                    getLocationOnScreen(location);
                    Point size = DisplayUtils.getScreenSize(getContext());
                    showBitmap = Bitmap.createBitmap(bitmap, location[0], location[1], size.x - location[0], size.y - location[1]);
                    if (colorNode.isValid()){
                        markArea = binder.matchColor(showBitmap, colorNode.getValue().getColor());
                        if (markArea != null && markArea.size() > 0) {
                            isMarked = true;
                            binding.seekBar.setMax(markArea.size());
                            binding.seekBar.setProgress(markArea.size());
                        }
                    }
                    refreshUI();
                    bitmap.recycle();
                }
            }
        }, delay);
    }

    public void onShow(){
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
            if (service.binder == null){
                Toast.makeText(getContext(), R.string.capture_service_on_tips_2, Toast.LENGTH_SHORT).show();
                service.startCaptureService(true, result -> {
                    if (result){
                        binder = service.binder;
                        realShow(500);
                    }
                });
            } else {
                binder = service.binder;
                realShow(100);
            }
        } else {
            EasyFloat.show(tag);
        }
    }

    @Override
    public void dispatchDraw(Canvas canvas) {
        if (showBitmap != null && !showBitmap.isRecycled()){
            canvas.drawBitmap(showBitmap, 0, 0, bitmapPaint);
        }
        canvas.saveLayer(getLeft(), getTop(), getRight(), getBottom(), bitmapPaint);
        long drawingTime = getDrawingTime();
        drawChild(canvas, binding.getRoot(), drawingTime);
        for (int i = 0; i < markArea.size(); i++) {
            if (((float) i) / markArea.size() < ((float) value) / binding.seekBar.getMax()){
                canvas.drawRect(markArea.get(i), markPaint);
            }
        }
        canvas.restore();
        if (isMarked){
            drawChild(canvas, binding.buttonBox, drawingTime);
            if (binding.seekBar.getMax() != 1) drawChild(canvas, binding.seekBar, drawingTime);
        }
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        float x = event.getX();
        float y = event.getY();

        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                if (!isMarked) {
                    lastX = x;
                    lastY = y;
                }
                break;
            case MotionEvent.ACTION_MOVE:
                if (!isMarked){
                    float dx = x - lastX;
                    float dy = y - lastY;
                    if (!drag && dx * dx + dy * dy < 81) break;
                    drag = true;
                    lastX = x;
                    lastY = y;
                }
                break;
            case MotionEvent.ACTION_UP:
                if (!isMarked && !drag && binder != null){
                    color = DisplayUtils.getHsvColor(showBitmap, (int) lastX, (int) lastY);
                    markArea = binder.matchColor(showBitmap, color);
                    if (markArea != null && markArea.size() > 0) {
                        isMarked = true;
                        binding.seekBar.setMax(markArea.size());
                        binding.seekBar.setProgress(markArea.size());
                    }
                }
                drag = false;
                break;
        }
        refreshUI();
        return true;
    }

    private void refreshUI(){
        binding.buttonBox.setVisibility(isMarked ? VISIBLE : INVISIBLE);
        binding.seekBar.setVisibility(isMarked ? VISIBLE : INVISIBLE);
        postInvalidate();
    }

    protected class ImagePickerCallback extends FloatBaseCallback{
        private boolean first = true;
        @Override
        public void onShow(String tag) {
            if (first){
                super.onShow("");
                ColorPickerFloatView.this.onShow();
                first = false;
            }
        }
    }
}
