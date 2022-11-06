package top.bogey.touch_tool.ui.play;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.databinding.FloatPlayBinding;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;
import top.bogey.touch_tool.utils.easy_float.SidePattern;

@SuppressLint("ViewConstructor")
public class PlayFloatView extends FrameLayout implements FloatViewInterface {
    private final FloatPlayBinding binding;
    private boolean clickFirst = false;

    public PlayFloatView(@NonNull Context context, String pkgName) {
        super(context);

        binding = FloatPlayBinding.inflate(LayoutInflater.from(context), this, true);
        binding.closeButton.setOnClickListener(v -> {
            if (clickFirst) dismiss();
            else {
                clickFirst = true;
                postDelayed(() -> clickFirst = false, 500);
                ViewGroup.LayoutParams params = binding.closeButton.getLayoutParams();
                if (binding.buttonBox.getVisibility() == VISIBLE) {
                    binding.buttonBox.setVisibility(GONE);
                    binding.closeButton.setIconResource(R.drawable.icon_down);

                    params.height = DisplayUtils.dp2px(context, 32);
                } else {
                    binding.buttonBox.setVisibility(VISIBLE);
                    binding.closeButton.setIconResource(R.drawable.icon_up);

                    params.height = DisplayUtils.dp2px(context, 24);
                }
                binding.closeButton.setLayoutParams(params);
            }
        });

        setPkgName(pkgName);
    }

    @Override
    public void show() {
        dismiss();
        EasyFloat.with(getContext())
                .setLayout(this)
                .setSidePattern(SidePattern.HORIZONTAL)
                .setGravity(FloatGravity.RIGHT_CENTER, 0, 0)
                .setBorder(20, 20, 0, 0)
                .setTag(PlayFloatView.class.getCanonicalName())
                .setAlwaysShow(true)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(PlayFloatView.class.getCanonicalName());
    }

    public void setPkgName(String pkgName) {
        for (int i = 0; i < binding.buttonBox.getChildCount(); i++) {
            PlayFloatViewItem view = (PlayFloatViewItem) binding.buttonBox.getChildAt(i);
            if (view.isPlaying()) view.startPlay();
        }
        binding.buttonBox.removeAllViews();
        List<Task> tasks = getManualTasks(pkgName);
        for (Task task : tasks) {
            binding.buttonBox.addView(new PlayFloatViewItem(getContext(), task));
        }
    }

    private List<Task> getManualTasks(String pkgName) {
        List<Task> tasks = new ArrayList<>();
        return tasks;
    }
}
