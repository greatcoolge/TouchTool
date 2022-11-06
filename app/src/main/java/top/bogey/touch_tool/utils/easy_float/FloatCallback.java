package top.bogey.touch_tool.utils.easy_float;

import android.view.MotionEvent;

public interface FloatCallback {
    void onCreate(boolean succeed);

    void onShow(String tag);

    void onHide();

    void onDismiss();

    void onTouch(MotionEvent event);

    void onDrag(MotionEvent event);

    void onDragEnd();
}
