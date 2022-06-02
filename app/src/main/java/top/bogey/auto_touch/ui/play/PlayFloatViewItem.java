package top.bogey.auto_touch.ui.play;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatPlayItemBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.node.Node;
import top.bogey.auto_touch.room.bean.node.NodeType;
import top.bogey.auto_touch.room.data.TaskRunnable;
import top.bogey.auto_touch.utils.TaskCallback;

@SuppressLint("ViewConstructor")
public class PlayFloatViewItem extends FrameLayout {
    private final FloatPlayItemBinding binding;
    private final Task task;

    private boolean playing = false;
    private TaskRunnable taskRunnable;

    public PlayFloatViewItem(@NonNull Context context, Task task) {
        super(context);
        this.task = task;

        binding = FloatPlayItemBinding.inflate(LayoutInflater.from(context), this, true);

        binding.playButton.setLabelText(getPivotalTitle(task.getTitle()));

        binding.playButton.setOnClickListener(v -> {
            MainAccessibilityService service = MainApplication.getService();
            // 录屏服务没开启，需要检查点击图片的动作
            if (service.binder == null){
                List<Action> actions = task.getActions();
                boolean flag = false;
                for (Action action : actions) {
                    for (Node target : action.getTargets()) {
                        if (target.getType() == NodeType.IMAGE){
                            flag = true;
                            break;
                        }
                    }
                    if (flag) break;
                    if (action.getCondition() != null && action.getCondition().getType() == NodeType.IMAGE){
                        flag = true;
                        break;
                    }
                }
                if (flag){
                    Toast.makeText(context, R.string.capture_service_on_tips_2, Toast.LENGTH_LONG).show();
                    service.startCaptureService(true, result -> {
                        if (result){
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

    private void startPlay(){
        MainAccessibilityService service = MainApplication.getService();
        if (service != null && service.isServiceConnected()){
            if (playing){
                if (taskRunnable != null && taskRunnable.isRunning()){
                    taskRunnable.stop();
                }
                playing = false;
            } else {
                taskRunnable = service.runTask(task, new TaskCallback() {
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
            }
            refreshProgress(0);
        }
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

    private synchronized void refreshProgress(int percent){
        post(() -> {
            binding.playButton.setLabelText(percent == 0 ? getPivotalTitle(task.getTitle()) : String.valueOf(percent));
            binding.playButton.showAnimation(binding.playButton.getProgress(), percent, 100);
        });
    }

}
