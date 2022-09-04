package top.bogey.touch_tool.utils.easy_float;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Point;
import android.os.Handler;
import android.os.Looper;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager.LayoutParams;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.utils.AppUtils;

public class EasyFloat {
    private static final String TAG = "DEFAULT_TAG";
    public static final int FOCUSABLE = LayoutParams.FLAG_NOT_TOUCH_MODAL | LayoutParams.FLAG_LAYOUT_NO_LIMITS | LayoutParams.FLAG_SHOW_WHEN_LOCKED;
    public static final int NOT_FOCUSABLE = LayoutParams.FLAG_NOT_TOUCH_MODAL | LayoutParams.FLAG_NOT_FOCUSABLE | LayoutParams.FLAG_LAYOUT_NO_LIMITS | LayoutParams.FLAG_SHOW_WHEN_LOCKED;

    private static final HashMap<String, FloatViewHelper> views = new HashMap<>();
    private static final List<String> tags = new ArrayList<>();

    public static Builder with(Context context){
        return new Builder(context);
    }

    public static void dismiss(String tag){
        tag = checkTag(tag);
        if (tags.contains(tag)){
            FloatViewHelper helper = getHelper(tag);
            helper.exitAnim();
            views.remove(tag);
            tags.remove(tag);
        }
    }

    public static void hide(String tag){
        tag = checkTag(tag);
        if (tags.contains(tag)){
            FloatViewHelper helper = getHelper(tag);
            helper.floatViewParent.setVisibility(View.INVISIBLE);
            if (helper.config.callback != null){
                helper.config.callback.onHide();
            }
        }
    }

    public static void hideAll(String ignored){
        for (String tag : tags) {
            if (tag.equals(ignored)) continue;
            FloatViewHelper helper = getHelper(tag);
            if (helper.config.alwaysShow) continue;
            hide(tag);
        }
    }

    public static boolean showLast(){
        for (int i = tags.size() - 1; i >= 0; i--) {
            FloatViewHelper helper = getHelper(tags.get(i));
            if (helper.config.alwaysShow) continue;
            show(tags.get(i));
            return true;
        }
        return false;
    }

    public static void show(String tag){
        tag = checkTag(tag);
        if (tags.contains(tag)){
            FloatViewHelper helper = getHelper(tag);
            helper.floatViewParent.setVisibility(View.VISIBLE);
            if (helper.config.callback != null){
                helper.config.callback.onShow(helper.config.tag);
            }
        }
    }

    public static View getView(String tag){
        tag = checkTag(tag);
        if (tags.contains(tag)){
            FloatViewHelper helper = getHelper(tag);
            return helper.getView();
        }
        return null;
    }

    @SuppressLint("ClickableViewAccessibility")
    public static void initInput(EditText editText, String tag){
        editText.setOnTouchListener((v, event) -> {
            if (event.getAction() == MotionEvent.ACTION_DOWN){
                openInput(editText, tag);
            }
            return false;
        });
    }

    public static void openInput(EditText editText, String tag){
        FloatViewHelper helper = getHelper(tag);
        if (helper != null){
            helper.params.flags = FOCUSABLE;
            helper.manager.updateViewLayout(helper.floatViewParent, helper.params);
        }

        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            InputMethodManager inputManager = (InputMethodManager) editText.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
            if (inputManager != null){
                inputManager.showSoftInput(editText, 0);
            }
        }, 100);
    }

    static FloatViewHelper getHelper(String tag){
        return views.get(tag);
    }

    private static String checkTag(String tag){
        if (tag == null || tag.isEmpty()) tag = TAG;
        return tag;
    }

    public static class Builder {
        private final Context context;
        private final FloatConfig config = new FloatConfig();

        public Builder(Context context) {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null) this.context = service;
            else this.context = context;
        }

        public Builder setLayout(int layoutId){
            config.layoutId = layoutId;
            return this;
        }

        public Builder setLayout(View layoutView){
            config.layoutView = layoutView;
            return this;
        }

        public Builder setTag(String tag){
            config.tag = tag;
            return this;
        }

        public Builder setDragEnable(boolean dragEnable){
            config.dragEnable = dragEnable;
            return this;
        }

        public Builder hasEditText(boolean hasEditText){
            config.hasEditText = hasEditText;
            return this;
        }

        public Builder setSidePattern(SidePattern side){
            config.side = side;
            return this;
        }

        public Builder setMatch(boolean matchWidth, boolean matchHeight){
            config.matchWidth = matchWidth;
            config.matchHeight = matchHeight;
            return this;
        }

        public Builder setGravity(FloatGravity gravity, int x, int y){
            config.gravity = gravity;
            config.offset = new Point(x, y);
            return this;
        }

        public Builder setBorder(int left, int right, int top, int bottom){
            config.leftBorder = left;
            config.rightBorder = right;
            config.topBorder = top;
            config.bottomBorder = bottom;
            return this;
        }

        public Builder setCallback(FloatCallback callback){
            config.callback = callback;
            return this;
        }

        public Builder setAnimator(FloatAnimator animator){
            config.animator = animator;
            return this;
        }

        public Builder setAlwaysShow(boolean alwaysShow){
            config.alwaysShow = alwaysShow;
            return this;
        }

        public void show(){
            if (config.layoutId == 0 && config.layoutView == null){
                if (config.callback != null){
                    config.callback.onCreate(false);
                }
                return;
            }
            if (config.tag == null || config.tag.isEmpty())
                config.tag = TAG;

            if (AppUtils.checkFloatPermission(context)){
                createFloatView();
            } else {
                MainActivity activity = MainApplication.getActivity();
                activity.launchFloat(null);
            }
        }

        private void createFloatView(){
            if (tags.contains(config.tag)){
                if (config.callback != null){
                    config.callback.onCreate(false);
                }
                return;
            }
            FloatViewHelper helper = new FloatViewHelper(context, config);
            helper.createView();
            views.put(config.tag, helper);
            tags.add(config.tag);
        }
    }
}

