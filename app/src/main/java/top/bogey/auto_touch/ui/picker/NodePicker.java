package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.FrameLayout;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.util.AppUtil;

@SuppressLint("ViewConstructor")
public class NodePicker extends FrameLayout implements NodePickerInterface {
    protected final PickerCallback pickerCallback;
    protected FloatShowPickerCallback floatCallback = null;

    protected NodePicker(Context context, PickerCallback pickerCallback) {
        super(context);
        this.pickerCallback = pickerCallback;
    }

    @Override
    public void show(){
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setTag(AppUtil.getIdentityCode(this))
                .setDragEnable(false)
                .setMatch(true, true)
                .setCallback(floatCallback)
                .setAnimator(null)
                .show();
    }

    @Override
    public void dismiss(){
        EasyFloat.dismiss(AppUtil.getIdentityCode(this));
    }
}
