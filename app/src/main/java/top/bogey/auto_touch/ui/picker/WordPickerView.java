package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentPickerWordBinding;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.easy_float.FloatGravity;
import top.bogey.auto_touch.util.AppUtil;

@SuppressLint("ViewConstructor")
public class WordPickerView extends FrameLayout implements NodePickerInterface {
    private final PickerCallback pickerCallback;
    private final FloatFragmentPickerWordBinding binding;
    private Rect rect;
    private boolean asNative;

    public WordPickerView(@NonNull Context context, PickerCallback pickerCallback) {
        super(context);
        this.pickerCallback = pickerCallback;
        binding = FloatFragmentPickerWordBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());
    }

    @Override
    public void show(int x, int y) {
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setTag(AppUtil.getIdentityCode(this))
                .setDragEnable(false)
                .setGravity(FloatGravity.TOP_LEFT, x, y)
                .setAnimator(null)
                .setCallback(new FloatClickCallback(() -> {
                    if (pickerCallback != null){
                        pickerCallback.call(this);
                    }
                    dismiss();
                })).show();
    }

    public void show(Rect rect, String key){
        show(rect.left, rect.top);
        init(rect, key, false);
    }

    public void init(Rect rect, String key, boolean asNative){
        binding.titleText.setText(simpleKey(key));
        ViewGroup.LayoutParams params = binding.getRoot().getLayoutParams();
        params.width = rect.width();
        params.height = rect.height();
        binding.getRoot().setLayoutParams(params);
        if (asNative){
            binding.getRoot().setOnClickListener(v -> {
                if (pickerCallback != null){
                    pickerCallback.call(this);
                }
            });
        }
        refreshSelectState(!asNative);
        this.rect = rect;
        this.asNative = asNative;
    }

    public Rect getRect() {
        return rect;
    }

    public void refreshSelectState(boolean isSelected){
        binding.getRoot().setBackgroundResource(isSelected ? R.drawable.border : R.drawable.dash_border);
        binding.titleText.setBackgroundResource(isSelected ? R.drawable.border : R.drawable.dash_border);
    }

    @Override
    public void dismiss() {
        if (!asNative){
            EasyFloat.dismiss(AppUtil.getIdentityCode(this));
        }
    }

    public String getWord(){
        return String.valueOf(binding.titleText.getText());
    }

    private String simpleKey(String key){
        Pattern pattern = Pattern.compile(".+:(id/.+)");
        Matcher matcher = pattern.matcher(key);
        if (matcher.find() && matcher.group(1) != null){
            key = matcher.group(1);
        }
        return key;
    }
}
