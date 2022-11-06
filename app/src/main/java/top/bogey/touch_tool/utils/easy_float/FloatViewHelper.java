package top.bogey.touch_tool.utils.easy_float;

import android.accessibilityservice.AccessibilityService;
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

import top.bogey.touch_tool.utils.DisplayUtils;

public class FloatViewHelper {
    Context context;
    FloatConfig config;

    FloatTouchUtils touchUtils = null;
    public WindowManager manager = null;
    public LayoutParams params = null;
    public FloatViewParent floatViewParent = null;

    Boolean isPortrait = null;

    public FloatViewHelper(Context context, FloatConfig config) {
        this.context = context;
        this.config = config;
    }

    void createView() {
        touchUtils = new FloatTouchUtils(context, config);
        manager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        params = new LayoutParams();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            if (context instanceof AccessibilityService) params.type = LayoutParams.TYPE_ACCESSIBILITY_OVERLAY;
            else params.type = LayoutParams.TYPE_APPLICATION_OVERLAY;
        } else {
            params.type = LayoutParams.TYPE_PHONE;
        }
        params.format = PixelFormat.RGBA_8888;
        params.gravity = Gravity.START | Gravity.TOP;
        params.flags = EasyFloat.NOT_FOCUSABLE;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
            params.layoutInDisplayCutoutMode = LayoutParams.LAYOUT_IN_DISPLAY_CUTOUT_MODE_SHORT_EDGES;
        }
        params.width = config.matchWidth ? LayoutParams.MATCH_PARENT : LayoutParams.WRAP_CONTENT;
        params.height = config.matchHeight ? LayoutParams.MATCH_PARENT : LayoutParams.WRAP_CONTENT;

        floatViewParent = new FloatViewParent(context, config);
        View floatInnerView = config.layoutView;
        if (floatInnerView == null) {
            floatInnerView = LayoutInflater.from(context).inflate(config.layoutId, floatViewParent, true);
            config.layoutView = floatInnerView;
        } else {
            floatViewParent.addView(floatInnerView);
        }

        floatViewParent.setVisibility(View.INVISIBLE);
        manager.addView(floatViewParent, params);
        floatViewParent.touchCallback = event -> touchUtils.updateFloatPosition(floatViewParent, event, manager, params);
        floatViewParent.layoutCallback = () -> {
            initGravity();

            isPortrait = DisplayUtils.isPortrait(context);

            if (config.callback != null) {
                config.callback.onCreate(true);
            }
            if (config.animator != null) {
                Animator animator = config.animator.enterAnim(floatViewParent, manager, params, config.side);
                if (animator != null) {
                    animator.addListener(new AnimatorListenerAdapter() {
                        @Override
                        public void onAnimationEnd(Animator animation) {
                            config.isAnim = false;
                            initEditText(floatViewParent);
                        }

                        @Override
                        public void onAnimationStart(Animator animation) {
                            config.isAnim = true;
                            floatViewParent.setVisibility(View.VISIBLE);
                            if (config.callback != null) {
                                config.callback.onShow(config.tag);
                            }
                        }

                        @Override
                        public void onAnimationCancel(Animator animation) {
                            config.isAnim = false;
                            initEditText(floatViewParent);
                        }
                    });
                    animator.start();
                }
            } else {
                floatViewParent.setVisibility(View.VISIBLE);
                initEditText(floatViewParent);
                if (config.callback != null) {
                    config.callback.onShow(config.tag);
                }
            }
        };

        setViewChangedListener();
    }

    private void setViewChangedListener() {
        floatViewParent.getViewTreeObserver().addOnGlobalLayoutListener(() -> {
            if (isPortrait == null || isPortrait.equals(DisplayUtils.isPortrait(context))) {
                return;
            }
            isPortrait = DisplayUtils.isPortrait(context);

            initGravity();
        });
    }

    public void initGravity() {
        Rect showSize = DisplayUtils.getScreenArea(context);
        int statusBarHeight = DisplayUtils.getStatusBarHeight(floatViewParent, params);
        showSize.top += config.topBorder;
        showSize.left += config.leftBorder;
        showSize.bottom -= (statusBarHeight + config.bottomBorder);
        showSize.right -= config.rightBorder;

        switch (config.gravity) {
            case TOP_LEFT:
                params.x = showSize.left;
                params.y = showSize.top;
                break;
            case TOP_CENTER:
                params.x = (showSize.width() - floatViewParent.getWidth()) / 2;
                params.y = showSize.top;
                break;
            case TOP_RIGHT:
                params.x = showSize.right - floatViewParent.getWidth();
                params.y = showSize.top;
                break;
            case LEFT_CENTER:
                params.x = showSize.left;
                params.y = (showSize.height() - floatViewParent.getHeight()) / 2;
                break;
            case CENTER:
                params.x = (showSize.width() - floatViewParent.getWidth()) / 2;
                params.y = (showSize.height() - floatViewParent.getHeight()) / 2;
                break;
            case RIGHT_CENTER:
                params.x = showSize.right - floatViewParent.getWidth();
                params.y = (showSize.height() - floatViewParent.getHeight()) / 2;
                break;
            case BOTTOM_LEFT:
                params.x = showSize.left;
                params.y = showSize.bottom - floatViewParent.getHeight();
                break;
            case BOTTOM_CENTER:
                params.x = (showSize.width() - floatViewParent.getWidth()) / 2;
                params.y = showSize.bottom - floatViewParent.getHeight();
                break;
            case BOTTOM_RIGHT:
                params.x = showSize.right - floatViewParent.getWidth();
                params.y = showSize.bottom - floatViewParent.getHeight();
                break;
        }
        params.x += config.offset.x;
        params.y += config.offset.y;

        manager.updateViewLayout(floatViewParent, params);
    }

    private void initEditText(View view) {
        if (config.hasEditText) {
            if (view instanceof ViewGroup) {
                ViewGroup viewGroup = (ViewGroup) view;
                for (int i = 0; i < viewGroup.getChildCount(); i++) {
                    initEditText(viewGroup.getChildAt(i));
                }
            } else {
                if (view instanceof EditText) {
                    EasyFloat.initInput((EditText) view, config.tag);
                }
            }
        }
    }

    View getView() {
        return config.layoutView;
    }

    void exitAnim() {
        if (config.animator != null) {
            Animator animator = config.animator.exitAnim(floatViewParent, manager, params, config.side);
            if (animator != null) {
                animator.addListener(new AnimatorListenerAdapter() {
                    @Override
                    public void onAnimationStart(Animator animation) {
                        config.isAnim = true;
                    }

                    @Override
                    public void onAnimationCancel(Animator animation) {
                        config.isAnim = false;
                        remove();
                    }

                    @Override
                    public void onAnimationEnd(Animator animation) {
                        config.isAnim = false;
                        remove();
                    }
                });
                animator.start();
                return;
            }
        }
        remove();
    }

    private void remove() {
        try {
            manager.removeView(floatViewParent);
        } catch (Exception ignored) {
        }
    }
}
