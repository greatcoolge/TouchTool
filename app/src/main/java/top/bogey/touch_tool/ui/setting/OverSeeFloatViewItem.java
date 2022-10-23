package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.databinding.FloatOverseeItemBinding;
import top.bogey.touch_tool.room.data.TaskCallable;

@SuppressLint("ViewConstructor")
public class OverSeeFloatViewItem extends FrameLayout {
    private final FloatOverseeItemBinding binding;

    public OverSeeFloatViewItem(@NonNull Context context, TaskCallable taskCallable) {
        super(context);

        binding = FloatOverseeItemBinding.inflate(LayoutInflater.from(context), this, true);

        binding.percent.setText(getPivotalTitle(taskCallable.getTask().getTitle()));

        binding.playButton.setOnClickListener(v -> {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null){
                service.stopTask(taskCallable, true);
                refreshProgress(0);
            }
        });

        refreshProgress(taskCallable.getTaskProgress());
    }

    private String getPivotalTitle(String title){
        if (title == null || title.isEmpty()) return "?";
        Pattern pattern = Pattern.compile("[\"|“](.*)[\"|”]");
        Matcher matcher = pattern.matcher(title);
        if (matcher.find()){
            String group = matcher.group(1);
            if (group != null) return group.substring(0, 1);
        }
        return title.substring(0, 1);
    }

    public void refreshProgress(int percent){
        post(() -> binding.playButton.setProgress(percent, percent != 0));
    }

}
