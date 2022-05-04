package top.bogey.auto_touch.ui.easy_float;

import android.view.MotionEvent;

public interface FloatCallback {
    void onCreate(boolean succeed);
    void onShow();
    void onHide();
    void onDismiss();
    void onTouch(MotionEvent event);
    void onDrag(MotionEvent event);
    void onDragEnd();
}
