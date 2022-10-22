package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import top.bogey.touch_tool.databinding.FloatOverseeBinding;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;
import top.bogey.touch_tool.utils.easy_float.SidePattern;

@SuppressLint("ViewConstructor")
public class OverSeeFloatView extends FrameLayout implements FloatViewInterface {
    public static final String OVERSEE_MODE = "oversee_mode";

    private final FloatOverseeBinding binding;
    private OverseeMode overseeMode;

    public OverSeeFloatView(@NonNull Context context, OverseeMode mode) {
        super(context);
        overseeMode = mode;

        binding = FloatOverseeBinding.inflate(LayoutInflater.from(context), this, true);
    }

    @Override
    public void show() {
        dismiss();
        EasyFloat.with(getContext())
                .setLayout(this)
                .setSidePattern(SidePattern.HORIZONTAL)
                .setGravity(FloatGravity.LEFT_CENTER, 0, 0)
                .setBorder(20, 20, 0, 0)
                .setTag(OverSeeFloatView.class.getCanonicalName())
                .setAlwaysShow(true)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(OverSeeFloatView.class.getCanonicalName());
    }


}
