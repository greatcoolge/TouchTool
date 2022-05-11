package top.bogey.auto_touch.ui.easy_float;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.content.Context;
import android.graphics.PixelFormat;
import android.graphics.Rect;
import android.os.Build;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.WindowManager.LayoutParams;
import android.widget.EditText;

import top.bogey.auto_touch.util.AppUtil;

public class FloatViewHelper {
    Context context;
    FloatConfig config;

    FloatTouchUtils touchUtils = null;
    WindowManager manager = null;
    LayoutParams params = null;
    FloatView floatView = null;

    public FloatViewHelper(Context context, FloatConfig config) {
        this.context = context;
        this.config = config;
    }

    void createView(){
        touchUtils = new FloatTouchUtils(context, config);
        manager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        params = new LayoutParams();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            params.type = LayoutParams.TYPE_APPLICATION_OVERLAY;
        } else {
            params.type = LayoutParams.TYPE_PHONE;
        }
        params.format = PixelFormat.RGBA_8888;
        params.gravity = Gravity.START | Gravity.TOP;
        params.flags = FloatUtils.NOT_FOCUSABLE;
        params.width = config.matchWidth ? LayoutParams.MATCH_PARENT : LayoutParams.WRAP_CONTENT;
        params.height = config.matchHeight ? LayoutParams.MATCH_PARENT : LayoutParams.WRAP_CONTENT;

        floatView = new FloatView(context, config);
        View floatInnerView = config.layoutView;
        if (floatInnerView == null){
            floatInnerView = LayoutInflater.from(context).inflate(config.layoutId, floatView, true);
            config.layoutView = floatInnerView;
        } else {
            floatView.addView(floatInnerView);
        }

        floatView.setVisibility(View.INVISIBLE);
        manager.addView(floatView, params);
        floatView.touchCallback = event -> touchUtils.updateFloatPosition(floatView, event, manager, params);
        floatView.layoutCallback = () -> {
            initGravity();

            if (config.callback != null){
                config.callback.onCreate(true);
            }
            if (config.animator != null){
                Animator animator = config.animator.enterAnim(floatView, manager, params, config.side);
                if (animator != null){
                    animator.addListener(new AnimatorListenerAdapter() {
                        @Override
                        public void onAnimationEnd(Animator animation) {
                            config.isAnim = false;
                            initEditText(floatView);
                        }

                        @Override
                        public void onAnimationStart(Animator animation) {
                            config.isAnim = true;
                            floatView.setVisibility(View.VISIBLE);
                            if (config.callback != null){
                                config.callback.onShow();
                            }
                        }

                        @Override
                        public void onAnimationCancel(Animator animation) {
                            config.isAnim = false;
                            initEditText(floatView);
                        }
                    });
                    animator.start();
                }
            } else {
                floatView.setVisibility(View.VISIBLE);
                initEditText(floatView);
                if (config.callback != null){
                    config.callback.onShow();
                }
            }
        };
    }

    private void initGravity(){
        Rect showSize = AppUtil.getScreenArea(context);
        int statusBarHeight = AppUtil.getStatusBarHeight(floatView, params);
        showSize.top += config.topBorder;
        showSize.bottom -= (statusBarHeight + config.bottomBorder);
        showSize.left += config.leftBorder;
        showSize.right -= config.rightBorder;

        switch (config.gravity){
            case TOP_LEFT:
                params.x = showSize.left;
                params.y = showSize.top;
                break;
            case TOP_CENTER:
                params.x = (showSize.width() - floatView.getWidth()) / 2;
                params.y = showSize.top;
                break;
            case TOP_RIGHT:
                params.x = showSize.right - floatView.getWidth();
                params.y = showSize.top;
                break;
            case LEFT_CENTER:
                params.x = showSize.left;
                params.y = (showSize.height() - floatView.getHeight()) / 2;
                break;
            case CENTER:
                params.x = (showSize.width() - floatView.getWidth()) / 2;
                params.y = (showSize.height() - floatView.getHeight()) / 2;
                break;
            case RIGHT_CENTER:
                params.x = showSize.right - floatView.getWidth();
                params.y = (showSize.height() - floatView.getHeight()) / 2;
                break;
            case BOTTOM_LEFT:
                params.x = showSize.left;
                params.y = showSize.bottom - floatView.getHeight();
                break;
            case BOTTOM_CENTER:
                params.x = (showSize.width() - floatView.getWidth()) / 2;
                params.y = showSize.bottom - floatView.getHeight();
                break;
            case BOTTOM_RIGHT:
                params.x = showSize.right - floatView.getWidth();
                params.y = showSize.bottom - floatView.getHeight();
                break;
        }
        params.x += config.offset.x;
        params.y += config.offset.y;

        manager.updateViewLayout(floatView, params);
    }

    private void initEditText(View view){
        if (config.hasEditText){
            if (view instanceof ViewGroup){
                    ViewGroup viewGroup = (ViewGroup) view;
                    for (int i = 0; i < viewGroup.getChildCount(); i++) {
                        initEditText(viewGroup.getChildAt(i));
                    }
            } else {
                if (view instanceof EditText){
                    FloatUtils.initInput((EditText) view, config.tag);
                }
            }
        }
    }

    View getView(){
        return config.layoutView;
    }

    void exitAnim(AnimCallback callback){
        if (config.animator != null){
            Animator animator = config.animator.exitAnim(floatView, manager, params, config.side);
            if (animator != null){
                animator.addListener(new AnimatorListenerAdapter() {
                    @Override
                    public void onAnimationStart(Animator animation) {
                        config.isAnim = true;
                    }

                    @Override
                    public void onAnimationCancel(Animator animation) {
                        config.isAnim = false;
                        callback.onAnimEnd();
                        remove();
                    }

                    @Override
                    public void onAnimationEnd(Animator animation) {
                        config.isAnim = false;
                        callback.onAnimEnd();
                        remove();
                    }
                });
                animator.start();
                return;
            }
        }
        callback.onAnimEnd();
        remove();
    }

    private void remove(){
        try{
            manager.removeView(floatView);
        } catch (Exception ignored){}
    }

    interface AnimCallback{
        void onAnimEnd();
    }
}
