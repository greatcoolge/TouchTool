package top.bogey.touch_tool.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.PorterDuff;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.graphics.RectF;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;
import android.widget.Toast;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.action.ImageAction;
import top.bogey.touch_tool.databinding.FloatPickerImageBinding;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.FloatBaseCallback;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

@SuppressLint("ViewConstructor")
public class ImagePickerFloatView extends BasePickerFloatView {
    private enum AdjustMode {NONE, DRAG, TOP_LEFT, BOTTOM_RIGHT}

    private final ImageAction imageAction;

    private final FloatPickerImageBinding binding;

    private MainAccessibilityService service;

    private final Paint bitmapPaint;
    private Bitmap showBitmap;

    private final Paint markPaint;
    public RectF markArea = new RectF();

    private AdjustMode adjustMode = AdjustMode.NONE;
    private boolean isMarked = false;

    private float lastX = 0;
    private float lastY = 0;

    private final int offset;

    public ImagePickerFloatView(Context context, PickerCallback pickerCallback, ImageAction imageAction) {
        super(context, pickerCallback);
        this.imageAction = imageAction;

        floatCallback = new ImagePickerCallback();

        binding = FloatPickerImageBinding.inflate(LayoutInflater.from(context), this, true);

        binding.saveButton.setOnClickListener(v -> {
            pickerCallback.onComplete(this);
            dismiss();
        });

        binding.backButton.setOnClickListener(v -> dismiss());

        markPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        markPaint.setStyle(Paint.Style.FILL);
        markPaint.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.DST_OUT));

        bitmapPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        bitmapPaint.setFilterBitmap(true);
        bitmapPaint.setDither(true);

        offset = DisplayUtils.dp2px(context, 4);
    }

    public Bitmap getBitmap() {
        if (showBitmap != null) {
            Bitmap bitmap = Bitmap.createBitmap(showBitmap, (int) markArea.left, (int) markArea.top, (int) markArea.width(), (int) markArea.height());
            showBitmap.recycle();
            return bitmap;
        }
        return null;
    }

    public void realShow(int delay) {
        postDelayed(() -> {
            EasyFloat.show(tag);
            if (service != null && service.isCaptureEnabled() && service.binder != null) {
                Bitmap bitmap = service.binder.getCurrImage();
                if (bitmap != null) {
                    int[] location = new int[2];
                    getLocationOnScreen(location);
                    Point size = DisplayUtils.getScreenSize(service);
                    if (bitmap.getWidth() >= size.x && bitmap.getHeight() >= size.y) {
                        showBitmap = Bitmap.createBitmap(bitmap, location[0], location[1], size.x - location[0], size.y - location[1]);
                        int width = DisplayUtils.getScreen(service);
                        float scale = ((float) width) / imageAction.getScreen();
                        Rect rect = service.binder.matchImage(showBitmap, imageAction.getScaleBitmap(scale), imageAction.getValue());
                        if (rect != null) {
                            markArea = new RectF(rect);
                            isMarked = true;
                        }
                    }
                    refreshUI();
                    bitmap.recycle();
                }
            }
        }, delay);
    }

    public void onShow() {
        service = MainApplication.getService();
        if (service != null) {
            if (!service.isCaptureEnabled()) {
                Toast.makeText(getContext(), R.string.capture_service_on_tips_2, Toast.LENGTH_SHORT).show();
                service.startCaptureService(true, result -> {
                    if (result) {
                        realShow(500);
                    }
                });
            } else {
                realShow(100);
            }
        } else {
            Toast.makeText(getContext(), R.string.capture_service_on_tips_3, Toast.LENGTH_SHORT).show();
            dismiss();
        }
    }

    @Override
    public void dispatchDraw(Canvas canvas) {
        if (showBitmap != null && !showBitmap.isRecycled()) {
            canvas.drawBitmap(showBitmap, 0, 0, bitmapPaint);
        }
        canvas.saveLayer(getLeft(), getTop(), getRight(), getBottom(), bitmapPaint);
        super.dispatchDraw(canvas);
        RectF rectF = new RectF(markArea);
        rectF.sort();
        canvas.drawRect(rectF, markPaint);
        canvas.restore();
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
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
                            } else {
                                adjustMode = AdjustMode.NONE;
                                markArea = new RectF(x, y, x, y);
                                isMarked = false;
                            }
                        }
                    }
                } else {
                    adjustMode = AdjustMode.NONE;
                    markArea = new RectF(x, y, x, y);
                }
                break;
            case MotionEvent.ACTION_MOVE:
                if (isMarked) {
                    float dx = x - lastX;
                    float dy = y - lastY;
                    switch (adjustMode) {
                        case DRAG:
                            markArea.left = Math.max(0, markArea.left + dx);
                            markArea.top = Math.max(0, markArea.top + dy);
                            markArea.right = Math.min(getRight(), markArea.right + dx);
                            markArea.bottom = Math.min(getBottom(), markArea.bottom + dy);
                            break;
                        case TOP_LEFT:
                            markArea.left = Math.max(0, markArea.left + dx);
                            markArea.top = Math.max(0, markArea.top + dy);
                            break;
                        case BOTTOM_RIGHT:
                            markArea.right = Math.min(getRight(), markArea.right + dx);
                            markArea.bottom = Math.min(getBottom(), markArea.bottom + dy);
                            break;
                    }
                    markArea.sort();
                    lastX = x;
                    lastY = y;
                } else {
                    markArea.right = x;
                    markArea.bottom = y;
                }
                break;

            case MotionEvent.ACTION_UP:
                if (isMarked) {
                    adjustMode = AdjustMode.NONE;
                } else {
                    if (!(markArea.width() == 0 || markArea.height() == 0)) {
                        markArea.right = x;
                        markArea.bottom = y;
                        markArea.sort();
                        isMarked = true;
                    }
                }
                break;
        }
        refreshUI();
        return true;
    }

    private void refreshUI() {
        binding.markBox.setVisibility(isMarked ? VISIBLE : INVISIBLE);
        binding.buttonBox.setVisibility(isMarked ? VISIBLE : INVISIBLE);
        if (isMarked) {
            ViewGroup.LayoutParams params = binding.markBox.getLayoutParams();
            params.width = (int) markArea.width() + 2 * offset;
            params.height = (int) markArea.height() + 2 * offset;
            binding.markBox.setLayoutParams(params);

            binding.markBox.setX(markArea.left - offset);
            binding.markBox.setY(markArea.top - offset);

            binding.moveRight.setX(params.width - binding.moveRight.getWidth());
            binding.moveRight.setY(params.height - binding.moveRight.getHeight());

            binding.buttonBox.setX(markArea.left + (markArea.width() - binding.buttonBox.getWidth()) / 2);
            if (markArea.bottom + offset * 2 + binding.buttonBox.getHeight() > binding.getRoot().getHeight()) {
                binding.buttonBox.setY(markArea.top - offset * 2 - binding.buttonBox.getHeight());
            } else {
                binding.buttonBox.setY(markArea.bottom + offset * 2);
            }
        }
        postInvalidate();
    }

    protected class ImagePickerCallback extends FloatBaseCallback {
        private boolean first = true;

        @Override
        public void onShow(String tag) {
            if (first) {
                super.onShow("");
                ImagePickerFloatView.this.onShow();
                first = false;
            }
        }
    }
}
