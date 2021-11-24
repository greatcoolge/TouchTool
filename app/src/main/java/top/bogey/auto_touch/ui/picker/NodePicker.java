package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.annotation.LayoutRes;
import androidx.annotation.NonNull;

import com.lzf.easyfloat.EasyFloat;
import com.lzf.easyfloat.enums.ShowPattern;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.util.AppUtil;

public class NodePicker {
    protected final Context context;
    protected View layout;

    protected boolean picking;
    protected PickerCallback pickerCallback;
    protected FloatCallback floatCallback;

    protected NodePicker(@NonNull Context context, @LayoutRes int layoutId, PickerCallback pickerCallback) {
        this.context = context;
        layout = LayoutInflater.from(context).inflate(layoutId, null);
        this.pickerCallback = pickerCallback;
        floatCallback = new FloatCallback();
    }

    protected NodePicker(Context context, View layout, PickerCallback pickerCallback) {
        this.context = context;
        this.layout = layout;
        this.pickerCallback = pickerCallback;
        floatCallback = new FloatCallback();
    }

    public int[] getLocation(){
        int[] location = new int[2];
        layout.getLocationOnScreen(location);
        return location;
    }

    public void show(int gravity, int x, int y){
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(layout)
                .setShowPattern(ShowPattern.ALL_TIME)
                .setTag(AppUtil.getIdentityCode(this))
                .setDragEnable(true)
                .setImmersionStatusBar(true)
                .setGravity(gravity, x, y)
                .registerCallbacks(floatCallback)
                .show();
    }

    public void dismiss(){
        EasyFloat.dismiss(AppUtil.getIdentityCode(this));
    }

    public void setPicking(boolean picking){
        this.picking = picking;
        EasyFloat.dragEnable(picking, AppUtil.getIdentityCode(this));
    }
}
