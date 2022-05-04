package top.bogey.auto_touch.ui.picker;

import android.view.MotionEvent;
import android.view.View;

import androidx.annotation.NonNull;

import top.bogey.auto_touch.util.CompleteCallback;

public class FloatClickCallback extends FloatCallbackImpl {
    private final CompleteCallback clickCallback;
    private boolean drag;
    private float lastX, lastY;

    public FloatClickCallback(CompleteCallback clickCallback) {
        this.clickCallback = clickCallback;
    }

    @Override
    public void onTouch(MotionEvent motionEvent) {
        super.onTouch(motionEvent);
        float rawX = motionEvent.getRawX();
        float rawY = motionEvent.getRawY();
        switch (motionEvent.getAction()) {
            case MotionEvent.ACTION_DOWN:
                drag = false;
                lastX = rawX;
                lastY = rawY;
            case MotionEvent.ACTION_MOVE:
                float dx = rawX - lastX;
                float dy = rawY - lastY;
                if (!drag && dx * dx + dy * dy < 81) break;
                drag = true;
                lastX = rawX;
                lastY = rawY;
            case MotionEvent.ACTION_UP:
                if (!drag && clickCallback != null){
                    clickCallback.onComplete();
                }
        }
    }
}
