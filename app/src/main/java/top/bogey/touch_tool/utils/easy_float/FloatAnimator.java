package top.bogey.touch_tool.utils.easy_float;

import android.animation.Animator;
import android.animation.ValueAnimator;
import android.graphics.Rect;
import android.view.View;
import android.view.WindowManager;
import android.view.WindowManager.LayoutParams;

import top.bogey.touch_tool.utils.DisplayUtils;

class FloatAnimator {
    Animator enterAnim(View view, WindowManager manager, LayoutParams params, SidePattern side) {
        return getAnimator(view, manager, params, side, false);
    }

    Animator exitAnim(View view, WindowManager manager, LayoutParams params, SidePattern side) {
        return getAnimator(view, manager, params, side, true);
    }

    private Animator getAnimator(View view, WindowManager manager, LayoutParams params, SidePattern side, boolean isExit) {
        Rect showRect = DisplayUtils.getScreenArea(view.getContext());
        int leftDistance = params.x - showRect.left;
        int rightDistance = showRect.right - leftDistance - view.getWidth();
        int topDistance = params.y - showRect.top;
        int bottomDistance = showRect.bottom - topDistance - view.getHeight();

        boolean isX = true;
        int start = 0;
        int end = 0;
        switch (side) {
            case LEFT:
                start = showRect.left - view.getWidth();
                end = params.x;
                break;
            case RIGHT:
                start = showRect.right;
                end = params.x;
                break;
            case TOP:
                isX = false;
                start = showRect.top - view.getHeight();
                end = params.y;
                break;
            case BOTTOM:
                isX = false;
                start = showRect.bottom;
                end = params.y;
                break;
            case HORIZONTAL:
                start = leftDistance < rightDistance ? showRect.left - view.getWidth() : showRect.right;
                end = params.x;
                break;
            case VERTICAL:
                isX = false;
                start = topDistance < bottomDistance ? showRect.top - view.getHeight() : showRect.bottom;
                end = params.y;
                break;
            case SIDE:
            case DEFAULT:
                int minX = Math.min(leftDistance, rightDistance);
                int minY = Math.min(topDistance, bottomDistance);
                if (minX > minY) {
                    isX = false;
                    start = topDistance < bottomDistance ? showRect.top - view.getHeight() : showRect.bottom;
                    end = params.y;
                } else {
                    start = leftDistance < rightDistance ? showRect.left - view.getWidth() : showRect.right;
                    end = params.x;
                }
                break;
        }
        if (isExit) {
            int tmp = start;
            start = end;
            end = tmp;
        }
        ValueAnimator animator = ValueAnimator.ofInt(start, end);
        boolean finalIsX = isX;
        animator.addUpdateListener(animation -> {
            try {
                if (finalIsX) params.x = (int) animation.getAnimatedValue();
                else params.y = (int) animation.getAnimatedValue();
                manager.updateViewLayout(view, params);
            } catch (Exception ignored) {
                animation.cancel();
            }
        });
        return animator;
    }
}
