package top.bogey.touch_tool.utils.easy_float;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

@SuppressLint("ViewConstructor")
public class FloatViewParent extends FrameLayout {
    private final FloatConfig config;

    public LayoutCallback layoutCallback = null;
    private boolean isCreated = false;

    public TouchCallback touchCallback = null;

    public FloatViewParent(@NonNull Context context, FloatConfig config) {
        super(context, null, 0);
        this.config = config;
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        if (!isCreated) {
            isCreated = true;
            if (layoutCallback != null)
                layoutCallback.onLayout();
        }
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (event != null && touchCallback != null) touchCallback.onTouch(event);
        return config.isDrag || super.onTouchEvent(event);
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent event) {
        if (event != null && touchCallback != null) touchCallback.onTouch(event);
        return config.isDrag || super.onInterceptTouchEvent(event);
    }

    @Override
    public boolean dispatchKeyEventPreIme(KeyEvent event) {
        if (config.hasEditText) {
            if (event != null && event.getAction() == KeyEvent.ACTION_DOWN && event.getKeyCode() == KeyEvent.KEYCODE_BACK) {
                FloatViewHelper helper = EasyFloat.getHelper(config.tag);
                if (helper != null) {
                    helper.params.flags = EasyFloat.NOT_FOCUSABLE;
                    helper.manager.updateViewLayout(helper.floatViewParent, helper.params);
                }
            }
        }
        return super.dispatchKeyEventPreIme(event);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (config.callback != null) {
            config.callback.onDismiss();
        }
    }

    interface LayoutCallback {
        void onLayout();
    }

    interface TouchCallback {
        void onTouch(MotionEvent event);
    }
}
