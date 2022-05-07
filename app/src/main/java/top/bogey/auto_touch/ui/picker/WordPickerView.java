package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentPickerWordBinding;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.easy_float.FloatGravity;
import top.bogey.auto_touch.util.AppUtil;

@SuppressLint("ViewConstructor")
public class WordPickerView extends FrameLayout implements NodePickerInterface {
    private final FloatFragmentPickerWordBinding binding;

    private String key;
    private String level;

    public WordPickerView(@NonNull Context context, PickerCallback pickerCallback) {
        super(context);
        binding = FloatFragmentPickerWordBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());

        binding.getRoot().setOnClickListener(v -> {
            if (pickerCallback != null){
                pickerCallback.call(this);
                dismiss();
            }
        });

        binding.titleText.setOnClickListener(v -> {
            CharSequence text = binding.titleText.getText();
            if (text != null && text.length() > 0){
                if (text.toString().equals(key)){
                    binding.titleText.setText(level);
                } else {
                    binding.titleText.setText(key);
                }
            } else {
                binding.titleText.setText(key);
            }
        });
    }

    @Override
    public void show(int x, int y) {
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setTag(AppUtil.getIdentityCode(this))
                .setDragEnable(false)
                .setGravity(FloatGravity.TOP_LEFT, x, y)
                .setAnimator(null)
                .show();
    }

    public void show(Rect rect, String key, String level){
        show(rect.left, rect.top);
        init(rect, key, level);
    }

    public void init(Rect rect, String key, String level){
        this.level = "lv/" + level;
        if (key.isEmpty()){
            this.key = this.level;
        } else {
            this.key = simpleKey(key);
        }
        binding.titleText.setText(this.key);
        ViewGroup.LayoutParams params = binding.getRoot().getLayoutParams();
        params.width = Math.max(rect.width(), AppUtil.dp2px(getContext(), 30));
        params.height = Math.max(rect.height(), AppUtil.dp2px(getContext(), 40));
        binding.getRoot().setLayoutParams(params);
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(AppUtil.getIdentityCode(this));
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
