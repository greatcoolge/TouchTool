package top.bogey.auto_touch.ui.easy_float;

import android.app.Activity;
import android.content.Context;
import android.graphics.Point;
import android.view.View;

import java.util.HashMap;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;

public class EasyFloat {
    private static final String TAG = "DEFAULT_TAG";

    private static final HashMap<String, FloatViewHelper> views = new HashMap<>();

    public static Builder with(Context context){
        return new Builder(context);
    }

    public static void dismiss(String tag){
        tag = checkTag(tag);
        if (views.containsKey(tag)){
            FloatViewHelper helper = getHelper(tag);
            String finalTag = tag;
            helper.exitAnim(() -> views.remove(finalTag));
        }
    }

    public static void hide(String tag){
        tag = checkTag(tag);
        if (views.containsKey(tag)){
            FloatViewHelper helper = getHelper(tag);
            helper.floatView.setVisibility(View.INVISIBLE);
            if (helper.config.callback != null){
                helper.config.callback.onHide();
            }
        }
    }

    public static void show(String tag){
        tag = checkTag(tag);
        if (views.containsKey(tag)){
            FloatViewHelper helper = getHelper(tag);
            helper.floatView.setVisibility(View.VISIBLE);
            if (helper.config.callback != null){
                helper.config.callback.onShow();
            }
        }
    }

    public static View getView(String tag){
        tag = checkTag(tag);
        if (views.containsKey(tag)){
            FloatViewHelper helper = getHelper(tag);
            return helper.getView();
        }
        return null;
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
            this.context = context;
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

        public void show(){
            if (config.layoutId == 0 && config.layoutView == null){
                if (config.callback != null){
                    config.callback.onCreate(false);
                }
                return;
            }
            if (config.tag == null || config.tag.isEmpty())
                config.tag = TAG;

            if (FloatUtils.checkFloatPermission(context)){
                createFloatView();
            } else {
                MainActivity activity = MainApplication.getActivity();
                activity.launchFloat((code, data) -> {
                    if (code == Activity.RESULT_OK){
                        createFloatView();
                    }
                });
            }
        }

        private void createFloatView(){
            if (views.containsKey(config.tag)){
                if (config.callback != null){
                    config.callback.onCreate(false);
                }
                return;
            }
            FloatViewHelper helper = new FloatViewHelper(context, config);
            helper.createView();
            views.put(config.tag, helper);
        }
    }
}

