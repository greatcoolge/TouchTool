package top.bogey.touch_tool.ui.play;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.FloatPlayItemBinding;
import top.bogey.touch_tool.room.bean.Action;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.node.Node;
import top.bogey.touch_tool.room.bean.node.NodeType;
import top.bogey.touch_tool.room.data.TaskCallable;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.RunningUtils;
import top.bogey.touch_tool.utils.TaskCallback;

@SuppressLint("ViewConstructor")
public class PlayFloatViewItem extends FrameLayout {
    private final FloatPlayItemBinding binding;
    private final Task task;

    private boolean playing = false;
    private TaskCallable taskCallable;

    public PlayFloatViewItem(@NonNull Context context, Task task) {
        super(context);
        this.task = task;

        binding = FloatPlayItemBinding.inflate(LayoutInflater.from(context), this, true);

        binding.percent.setText(getPivotalTitle(task.getTitle()));

        binding.playButton.setOnClickListener(v -> {
            MainAccessibilityService service = MainApplication.getService();
            // 录屏服务没开启，需要检查点击图片的动作
            if (service.binder == null) {
                List<Action> actions = task.getActions();
                boolean flag = false;
                for (Action action : actions) {
                    for (Node target : action.getTargets()) {
                        if (target.getType() == NodeType.IMAGE || target.getType() == NodeType.COLOR) {
                            flag = true;
                            break;
                        }
                    }
                    if (flag) break;
                    if (action.getCondition() != null && action.getCondition().getType() == NodeType.IMAGE) {
                        flag = true;
                        break;
                    }
                }
                if (flag) {
                    Toast.makeText(context, R.string.capture_service_on_tips_2, Toast.LENGTH_LONG).show();
                    service.startCaptureService(true, result -> {
                        if (result) {
                            startPlay();
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
                if (taskCallable != null && taskCallable.isRunning()) {
                    service.stopTask(taskCallable);
                }
                playing = false;
            } else {
                taskCallable = service.runTask(task, new TaskCallback() {
                    @Override
                    public void onStart() {
                        playing = true;
                        refreshProgress(0);
                    }

                    @Override
                    public void onEnd(boolean succeed) {
                        playing = false;
                        refreshProgress(0);
                    }

                    @Override
                    public void onProgress(int percent) {
                        refreshProgress(percent);
                    }
                });
                RunningUtils.log(LogLevel.MIDDLE, service.getString(R.string.log_run_manual_task, task.getTitle()));
            }
            refreshProgress(0);
        }
    }

    public boolean isPlaying() {
        return playing;
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

}
