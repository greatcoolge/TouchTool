package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.utils.AppUtils;
import top.bogey.auto_touch.utils.FloatBaseCallback;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;
import top.bogey.auto_touch.utils.easy_float.FloatViewInterface;

@SuppressLint("ViewConstructor")
public class BasePickerFloatView extends FrameLayout implements FloatViewInterface {
    protected final String tag;
    protected final PickerCallback pickerCallback;
    protected FloatBaseCallback floatCallback = new FloatBaseCallback();

    public BasePickerFloatView(@NonNull Context context, PickerCallback pickerCallback) {
        super(context);
        this.pickerCallback = pickerCallback;
        tag = AppUtils.getIdentityCode(this);
    }

    @Override
    public void show() {
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setTag(tag)
                .setDragEnable(false)
                .setMatch(true, true)
                .setCallback(floatCallback)
                .setAnimator(null)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(AppUtils.getIdentityCode(this));
    }
}
