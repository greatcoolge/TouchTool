package top.bogey.touch_tool.ui.play;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunnable;
import top.bogey.touch_tool.databinding.FloatPlayItemBinding;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.LogUtils;
import top.bogey.touch_tool.utils.TaskRunningCallback;

@SuppressLint("ViewConstructor")
public class PlayFloatViewItem extends FrameLayout implements TaskRunningCallback {
    private FloatPlayItemBinding binding;
    private final Task task;
    private final boolean isOversee;
    private TaskRunnable runnable;

    private boolean playing = false;
    private boolean needRemove = false;

    public PlayFloatViewItem(Context context, TaskRunnable runnable) {
        super(context);
        task = runnable.getTask();
        isOversee = true;

        runnable.addCallback(this);
        playing = true;

        initView(context);
    }

    public PlayFloatViewItem(@NonNull Context context, Task task) {
        super(context);
        this.task = task;
        isOversee = false;

        initView(context);
    }

    private void initView(Context context) {
        binding = FloatPlayItemBinding.inflate(LayoutInflater.from(context), this, true);

        binding.percent.setText(getPivotalTitle(task.getTitle()));
        binding.oversee.setVisibility(isOversee ? VISIBLE : GONE);

        binding.playButton.setOnClickListener(v -> {
            MainAccessibilityService service = MainApplication.getService();
            // 录屏服务没开启，需要检查点击图片的动作
            if (service != null && !service.isCaptureEnabled()) {
                if (task.includeCaptureAction()) {
                    Toast.makeText(context, R.string.capture_service_on_tips_2, Toast.LENGTH_LONG).show();
                    service.startCaptureService(true, result -> {
                        if (result) {
                            postDelayed(this::startPlay, 500);
                        }
                    });
                } else {
                    startPlay();
                }
            } else {
                startPlay();
            }
        });

        refreshProgress(0);
    }

    public void startPlay() {
        MainAccessibilityService service = MainApplication.getService();
        if (service != null && service.isServiceConnected()) {
            if (playing) {
                if (runnable != null && runnable.isRunning()) {
                    service.stopTask(runnable, isOversee);
                }
                playing = false;
            } else {
                runnable = service.runTask(task, this);
                LogUtils.log(LogLevel.MIDDLE, service.getString(R.string.log_run_manual_task, task.getTitle()));
                playing = true;
            }
            refreshProgress(0);
        }
    }

    public boolean isFree() {
        return !playing;
    }

    public boolean isOversee() {
        return isOversee;
    }

    public Task getTask() {
        return task;
    }

    public void setNeedRemove(boolean needRemove) {
        this.needRemove = needRemove;
    }

    private String getPivotalTitle(String title) {
        if (title == null || title.isEmpty()) return "?";
        Pattern pattern = Pattern.compile("[\"|“](.*)[\"|”]");
        Matcher matcher = pattern.matcher(title);
        if (matcher.find()) {
            String group = matcher.group(1);
            if (group != null) return group.substring(0, 1);
        }
        return title.substring(0, 1);
    }

    private void refreshProgress(int percent) {
        post(() -> {
            binding.percent.setText(!playing && percent == 0 ? getPivotalTitle(task.getTitle()) : String.valueOf(percent));
            binding.playButton.setProgress(percent, percent != 0);
        });
    }

    @Override
    public void onStart(TaskRunnable runnable) {
        playing = true;
        refreshProgress(0);
    }

    @Override
    public void onEnd(TaskRunnable runnable, boolean succeed) {
        playing = false;
        refreshProgress(0);
        if (needRemove || isOversee) {
            post(() -> {
                ViewParent parent = getParent();
                if (parent != null) ((ViewGroup) parent).removeView(this);
            });
        }
    }

    @Override
    public void onProgress(TaskRunnable runnable, int percent) {
        refreshProgress(percent);
    }
}
