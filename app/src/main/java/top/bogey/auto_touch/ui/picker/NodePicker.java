package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.annotation.LayoutRes;
import androidx.annotation.NonNull;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.util.AppUtil;

public class NodePicker implements NodePickerInterface {
    protected final Context context;
    protected View layout;

    protected PickerCallback pickerCallback;
    protected FloatCallbackImpl floatCallbackImpl;

    protected NodePicker(@NonNull Context context, @LayoutRes int layoutId, PickerCallback pickerCallback) {
        this.context = context;
        layout = LayoutInflater.from(context).inflate(layoutId, null);
        this.pickerCallback = pickerCallback;
        floatCallbackImpl = new FloatPickerShowCallback();
    }

    protected NodePicker(Context context, View layout, PickerCallback pickerCallback) {
        this.context = context;
        this.layout = layout;
        this.pickerCallback = pickerCallback;
        floatCallbackImpl = new FloatPickerShowCallback();
    }

    @Override
    public void show(int x, int y){
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(layout)
                .setTag(AppUtil.getIdentityCode(this))
                .setDragEnable(false)
                .setMatch(true, true)
                .setCallback(floatCallbackImpl)
                .setAnimator(null)
                .show();
    }

    @Override
    public void dismiss(){
        EasyFloat.dismiss(AppUtil.getIdentityCode(this));
    }
}
