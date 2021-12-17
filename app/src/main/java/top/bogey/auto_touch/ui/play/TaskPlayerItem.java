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
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.RunningCallback;

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

        binding.playButton.setOnClickListener(v -> {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null && service.isEnable()){
                if (playing){
                    if (taskRunnable != null && taskRunnable.isRunning()){
                        taskRunnable.stop();
                    }
                    playing = false;
                    refreshProgress(task.getGroupId(), 0);
                } else {
                    taskRunnable = service.runTask(this.task, new RunningCallback() {
                        @Override
                        public void onResult(boolean result) {
                            playing = false;
                            refreshProgress(task.getGroupId(), 0);
                        }

                        @Override
                        public void onProgress(int groupId, int percent) {
                            if (playing){
                                refreshProgress(groupId, percent);
                            }
                        }
                    });
                    playing = true;
                    refreshProgress(task.getGroupId(), 1);
                }
            }
        });
    }

    public void setTask(Task task){
        this.task = task;
        binding.playButton.setLabelText(getPivotalTitle(task.getTitle()));
        playing = false;
        refreshProgress(task.getGroupId(), 0);
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

    private synchronized void refreshProgress(int groupId, int percent){
        post(() -> {
            int color = AppUtil.getGroupColor(getContext(), groupId);
            binding.playButton.setLabelTextColor(color);
            binding.playButton.setProgressColor(color);

            binding.playButton.showAnimation(binding.playButton.getProgress(), percent, 100);
        });
    }
}
