package top.bogey.auto_touch.ui.play;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentPlayBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.room.data.TaskRepository;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.easy_float.FloatGravity;
import top.bogey.auto_touch.ui.easy_float.SidePattern;
import top.bogey.auto_touch.ui.picker.NodePickerInterface;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.CompleteCallback;

@SuppressLint("ViewConstructor")
public class TaskPlayerDialog extends FrameLayout implements NodePickerInterface {
    private final FloatFragmentPlayBinding binding;
    private final List<List<Task>> tasksList;
    private boolean firstClick = false;
    private int index = 0;

    private final CompleteCallback callback;

    public TaskPlayerDialog(@NonNull Context context, String pkgName, CompleteCallback callback) {
        super(context);
        this.callback = callback;
        binding = FloatFragmentPlayBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());

        TaskRepository repository = new TaskRepository(context);
        List<Task> tasks = repository.getTasksByPackageName(pkgName);
        tasksList = fixTasks(tasks);

        binding.closeButton.setOnClickListener(v -> {
            if (firstClick) dismiss();
            else{
                firstClick = true;
                postDelayed(() -> firstClick = false, 500);
                index++;
                showCurrPage(index);
            }
        });
        showCurrPage(index);
    }

    @Override
    public void show() {
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setSidePattern(SidePattern.HORIZONTAL)
                .setGravity(FloatGravity.RIGHT_CENTER, 0, 0)
                .setBorder(20, 20, 0, 0)
                .setTag(AppUtil.getIdentityCode(this))
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(AppUtil.getIdentityCode(this));
        callback.onComplete();
    }

    private List<List<Task>> fixTasks(List<Task> tasks){
        Map<Integer, List<Task>> taskMap = new HashMap<>();
        for (Task task : tasks) {
            if (task.getTaskStatus() == TaskStatus.MANUAL){
                List<Task> list = taskMap.computeIfAbsent(task.getGroupId(), k -> new ArrayList<>());
                list.add(task);
            }
        }
        List<List<Task>> tasksList = new ArrayList<>();
        for (Map.Entry<Integer, List<Task>> listEntry : taskMap.entrySet()) {
            tasksList.add(listEntry.getValue());
        }
        return tasksList;
    }

    private void showCurrPage(int index){
        List<Task> tasks = tasksList.get(index % tasksList.size());
        int i;
        for (i = 0; i < tasks.size(); i++) {
            View child = binding.buttonBox.getChildAt(i);
            if (child != null){
                TaskPlayerItem item = (TaskPlayerItem) child;
                item.setTask(tasks.get(i));
                item.setVisibility(View.VISIBLE);
            } else {
                TaskPlayerItem item = new TaskPlayerItem(getContext(), tasks.get(i));
                binding.buttonBox.addView(item);
            }
        }
        for (; i < binding.buttonBox.getChildCount(); i++){
            binding.buttonBox.getChildAt(i).setVisibility(View.GONE);
        }
    }
}
