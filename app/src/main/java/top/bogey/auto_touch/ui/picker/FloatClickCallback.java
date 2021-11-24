package top.bogey.auto_touch.ui.picker;

import android.view.MotionEvent;
import android.view.View;

import androidx.annotation.NonNull;

public class FloatClickCallback extends FloatCallback{
    private final NodePicker nodePicker;
    private boolean drag;
    private float lastX, lastY;

    public FloatClickCallback(NodePicker nodePicker) {
        this.nodePicker = nodePicker;
    }

    @Override
    public void touchEvent(@NonNull View view, @NonNull MotionEvent motionEvent) {
        super.touchEvent(view, motionEvent);
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
                if (!drag && nodePicker.picking && nodePicker.pickerCallback != null){
                    nodePicker.pickerCallback.call(nodePicker);
                }
        }
    }
}
