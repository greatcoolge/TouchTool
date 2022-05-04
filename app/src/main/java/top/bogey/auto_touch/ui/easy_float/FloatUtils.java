package top.bogey.auto_touch.ui.easy_float;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.view.MotionEvent;
import android.view.WindowManager.LayoutParams;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;

import java.lang.reflect.Method;

public class FloatUtils {
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
        FloatViewHelper helper = EasyFloat.getHelper(tag);
        if (helper != null){
            helper.params.flags = LayoutParams.FLAG_NOT_TOUCH_MODAL;
            helper.manager.updateViewLayout(helper.floatView, helper.params);
        }

        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            InputMethodManager inputManager = (InputMethodManager) editText.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
            if (inputManager != null){
                inputManager.showSoftInput(editText, 0);
            }
        }, 100);
    }

    public static void closeInput(String tag){
        FloatViewHelper helper = EasyFloat.getHelper(tag);
        if (helper != null){
            helper.params.flags = LayoutParams.FLAG_NOT_TOUCH_MODAL | LayoutParams.FLAG_NOT_FOCUSABLE;
            helper.manager.updateViewLayout(helper.floatView, helper.params);
        }
    }

    public static boolean checkFloatPermission(Context context){
        try {
            Method canDrawOverlays = Settings.class.getDeclaredMethod("canDrawOverlays", Context.class);
            return (Boolean) canDrawOverlays.invoke(null, context);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }
}
