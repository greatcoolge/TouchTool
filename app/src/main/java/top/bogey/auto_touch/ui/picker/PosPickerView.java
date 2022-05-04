package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.WindowManager;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentPickerPosBinding;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.easy_float.FloatGravity;
import top.bogey.auto_touch.util.AppUtil;

@SuppressLint("ViewConstructor")
public class PosPickerView extends FrameLayout implements NodePickerInterface {
    private final PickerCallback pickerCallback;
    private final FloatFragmentPickerPosBinding binding;
    private final int offset;

    public PosPickerView(@NonNull Context context, PickerCallback pickerCallback) {
        super(context);
        this.pickerCallback = pickerCallback;
        binding = FloatFragmentPickerPosBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());
        offset = AppUtil.dp2px(context, 15);
    }

    @Override
    public void show(int x, int y){
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setTag(AppUtil.getIdentityCode(this))
                .setGravity(FloatGravity.TOP_LEFT, x - offset, y - offset)
                .setCallback(new FloatClickCallback(() -> {
                    if (pickerCallback != null){
                        pickerCallback.call(this);
                    }
                    dismiss();
                })).show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(AppUtil.getIdentityCode(this));
    }

    public void setIndex(int index){
        binding.indexText.setText(String.valueOf(index));
    }

    public int[] getLocation(){
        int[] location = new int[2];
        WindowManager.LayoutParams params = EasyFloat.getParams(AppUtil.getIdentityCode(this));
        if (params != null){
            location[0] += offset + params.x;
            location[1] += offset + params.y;
        }
        return location;
    }
}
