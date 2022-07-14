package top.bogey.auto_touch.ui.play;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatPlayBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.room.data.TaskRepository;
import top.bogey.auto_touch.utils.DisplayUtils;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;
import top.bogey.auto_touch.utils.easy_float.FloatGravity;
import top.bogey.auto_touch.utils.easy_float.FloatViewInterface;
import top.bogey.auto_touch.utils.easy_float.SidePattern;

@SuppressLint("ViewConstructor")
public class PlayFloatView extends FrameLayout implements FloatViewInterface {
    private boolean clickFirst = false;

    public PlayFloatView(@NonNull Context context, String pkgName) {
        super(context);

        FloatPlayBinding binding = FloatPlayBinding.inflate(LayoutInflater.from(context), this, true);
        binding.closeButton.setOnClickListener(v -> {
            if (clickFirst) dismiss();
            else{
                clickFirst = true;
                postDelayed(() -> clickFirst = false, 500);
                ViewGroup.LayoutParams params = binding.closeButton.getLayoutParams();
                if (binding.buttonBox.getVisibility() == VISIBLE){
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

        List<Task> tasks = getManualTasks(pkgName);
        for (Task task : tasks) {
            binding.buttonBox.addView(new PlayFloatViewItem(getContext(), task));
        }
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

    private List<Task> getManualTasks(String pkgName){
        TaskRepository repository = new TaskRepository(getContext());
        List<Task> tasks = repository.getTasksByPackageName(pkgName);
        for (int i = tasks.size() - 1; i >= 0; i--) {
            if (tasks.get(i).getStatus() != TaskStatus.MANUAL) tasks.remove(i);
        }
        return tasks;
    }
}
