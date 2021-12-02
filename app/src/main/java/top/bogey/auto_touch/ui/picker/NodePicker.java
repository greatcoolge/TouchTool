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

public class NodePicker implements NodePickerInterface {
    protected final Context context;
    protected View layout;

    protected PickerCallback pickerCallback;
    protected FloatCallback floatCallback;

    protected NodePicker(@NonNull Context context, @LayoutRes int layoutId, PickerCallback pickerCallback) {
        this.context = context;
        layout = LayoutInflater.from(context).inflate(layoutId, null);
        this.pickerCallback = pickerCallback;
        floatCallback = new FloatPickerShowCallback();
    }

    protected NodePicker(Context context, View layout, PickerCallback pickerCallback) {
        this.context = context;
        this.layout = layout;
        this.pickerCallback = pickerCallback;
        floatCallback = new FloatPickerShowCallback();
    }

    @Override
    public void show(int gravity, int x, int y){
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(layout)
                .setShowPattern(ShowPattern.ALL_TIME)
                .setTag(AppUtil.getIdentityCode(this))
                .setDragEnable(false)
                .setImmersionStatusBar(true)
                .setGravity(gravity, x, y)
                .setMatchParent(true, true)
                .registerCallbacks(floatCallback)
                .setAnimator(null)
                .show();
    }

    @Override
    public void dismiss(){
        EasyFloat.dismiss(AppUtil.getIdentityCode(this));
    }
}
