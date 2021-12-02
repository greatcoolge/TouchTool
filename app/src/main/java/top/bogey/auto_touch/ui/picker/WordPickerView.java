package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import com.lzf.easyfloat.EasyFloat;
import com.lzf.easyfloat.enums.ShowPattern;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentPickerWordBinding;
import top.bogey.auto_touch.util.AppUtil;

@SuppressLint("ViewConstructor")
public class WordPickerView extends FrameLayout implements NodePickerInterface {
    private final PickerCallback pickerCallback;
    private final FloatFragmentPickerWordBinding binding;

    public WordPickerView(@NonNull Context context, PickerCallback pickerCallback) {
        super(context);
        this.pickerCallback = pickerCallback;
        binding = FloatFragmentPickerWordBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());
    }

    @Override
    public void show(int gravity, int x, int y) {
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setShowPattern(ShowPattern.ALL_TIME)
                .setTag(AppUtil.getIdentityCode(this))
                .setDragEnable(true)
                .setImmersionStatusBar(true)
                .setGravity(gravity, x, y)
                .setAnimator(null)
                .registerCallbacks(new FloatClickCallback(() -> {
                    if (pickerCallback != null){
                        pickerCallback.call(this);
                    }
                    dismiss();
                })).show();
    }

    public void show(Rect rect, String key){
        show(Gravity.START | Gravity.TOP, rect.left, rect.top);
        binding.titleText.setText(key);
        ViewGroup.LayoutParams params = binding.getRoot().getLayoutParams();
        params.width = rect.width();
        params.height = rect.height();
        binding.getRoot().setLayoutParams(params);
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(AppUtil.getIdentityCode(this));
    }

    public String getWord(){
        return String.valueOf(binding.titleText.getText());
    }

}
