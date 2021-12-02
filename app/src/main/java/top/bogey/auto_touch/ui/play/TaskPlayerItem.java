package top.bogey.auto_touch.ui.play;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentPlayItemBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.data.TaskRunnable;

@SuppressLint("ViewConstructor")
public class TaskPlayerItem extends FrameLayout {
    private final FloatFragmentPlayItemBinding binding;
    private Task task;
    private boolean playing = false;
    private TaskRunnable taskRunnable;

    public TaskPlayerItem(@NonNull Context context, Task task) {
        super(context);
        binding = FloatFragmentPlayItemBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());

        setTask(task);

        binding.getRoot().setOnClickListener(v -> {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null && service.enable){
                if (playing){
                    if (taskRunnable != null && taskRunnable.isRunning()){
                        taskRunnable.stop();
                    }
                    playing = false;
                } else {
                    taskRunnable = service.runTask(this.task, () -> {
                        playing = false;
                        refreshButton(false);
                    });
                    playing = true;
                }
                refreshButton(playing);
            }
        });
    }

    public void setTask(Task task){
        this.task = task;
        binding.getRoot().setText(getPivotalTitle(task.title));
        playing = false;
        refreshButton(false);
    }

    private String getPivotalTitle(String title){
        if (title == null || title.isEmpty()) return "";
        Pattern pattern = Pattern.compile("[\"|“](.*)[\"|”]");
        Matcher matcher = pattern.matcher(title);
        if (matcher.find()){
            String group = matcher.group(1);
            if (group != null) return group.substring(0, 1);
        }
        return title.substring(0, 1);
    }

    private synchronized void refreshButton(boolean playing){
        post(() -> {
            int[] attrs = new int[] {R.attr.colorOnPrimary};
            TypedArray typedArray = getContext().getTheme().obtainStyledAttributes(attrs);
            int selectColor = typedArray.getResourceId(0, R.color.grey_50);
            typedArray.recycle();

            attrs = new int[] {R.attr.colorPrimary};
            typedArray = getContext().getTheme().obtainStyledAttributes(attrs);
            int unselectColor = typedArray.getResourceId(0, R.color.teal_500);
            typedArray.recycle();

            if (playing){
                binding.getRoot().setBackgroundResource(R.drawable.corner);
                binding.getRoot().setTextColor(getResources().getColor(selectColor, null));
            } else {
                binding.getRoot().setBackgroundDrawable(null);
                binding.getRoot().setTextColor(getResources().getColor(unselectColor, null));
            }
        });
    }
}
