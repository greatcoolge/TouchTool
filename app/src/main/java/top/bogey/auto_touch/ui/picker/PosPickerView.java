package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import com.lzf.easyfloat.EasyFloat;
import com.lzf.easyfloat.enums.ShowPattern;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.databinding.FloatFragmentPickerPosBinding;

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
    public void show(int gravity, int x, int y){
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setShowPattern(ShowPattern.ALL_TIME)
                .setTag(AppUtil.getIdentityCode(this))
                .setDragEnable(true)
                .setImmersionStatusBar(true)
                .setGravity(gravity, x - offset, y - offset)
                .registerCallbacks(new FloatClickCallback(() -> {
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
        getLocationOnScreen(location);
        location[0] += offset;
        location[1] += offset;
        return location;
    }
}
