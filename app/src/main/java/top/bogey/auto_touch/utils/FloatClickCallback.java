package top.bogey.auto_touch.utils;

import android.view.MotionEvent;

public class FloatClickCallback extends FloatBaseCallback{
    private boolean drag;
    private float lastX, lastY;
    private final ClickCallback callback;

    public FloatClickCallback(ClickCallback callback) {
        this.callback = callback;
    }

    @Override
    public void onTouch(MotionEvent event) {
        float rawX = event.getRawX();
        float rawY = event.getRawY();
        switch (event.getAction()) {
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
                if (!drag && callback != null){
                    callback.onClicked((int) rawX, (int) rawY);
                }
        }
    }

    public interface ClickCallback {
        void onClicked(int rawX, int rawY);
    }
}
