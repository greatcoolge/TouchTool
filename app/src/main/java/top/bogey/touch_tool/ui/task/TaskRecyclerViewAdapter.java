package top.bogey.touch_tool.ui.task;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskConfig;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTaskItemBinding;
import top.bogey.touch_tool.ui.setting.SettingSave;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;

public class TaskRecyclerViewAdapter extends RecyclerView.Adapter<TaskRecyclerViewAdapter.ViewHolder> {
    private final List<Task> tasks = new ArrayList<>();
    private final TaskView parent;

    private boolean isCheck = false;
    private final Map<String, Task> selectTasks = new HashMap<>();

    private final TaskRepository repository = TaskRepository.getInstance();
    private SortType sortType = SettingSave.getInstance().getSortType();
    private final Comparator<Task> comparator = (o1, o2) -> {
        TaskConfig config1 = repository.getTaskConfig(o1.getId());
        TaskConfig config2 = repository.getTaskConfig(o2.getId());
        switch (sortType) {
            case CREATE_TIME_ASC:
                return (config1.getCreateTime() - config2.getCreateTime()) > 0 ? 1 : -1;
            case CREATE_TIME_DESC:
                return (config1.getCreateTime() - config2.getCreateTime()) > 0 ? -1 : 1;
            case MODIFY_TIME_ASC:
                return (config1.getModifyTime() - config2.getModifyTime()) > 0 ? 1 : -1;
            case MODIFY_TIME_DESC:
                return (config1.getModifyTime() - config2.getModifyTime()) > 0 ? -1 : 1;
        }
        return 0;
    };

    private final String ALL;
    private final String NO;

    public TaskRecyclerViewAdapter(TaskView parent) {
        this.parent = parent;
        tasks.addAll(TaskRepository.getInstance().getAllTasks());
        tasks.sort(comparator);
        ALL = parent.getString(R.string.tag_all);
        NO = parent.getString(R.string.tag_no);
    }

    public void setSortType(SortType type) {
        sortType = type;
        SettingSave.getInstance().setSortType(sortType);
        tasks.sort(comparator);
        notifyItemRangeChanged(0, tasks.size());
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewTaskItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, int position) {
        holder.refreshItem(tasks.get(position));
    }

    @Override
    public int getItemCount() {
        return tasks.size();
    }

    public boolean isCheck() {
        return isCheck;
    }

    public void onChanged(Task task) {
        boolean flag = true;
        for (int i = 0; i < tasks.size(); i++) {
            if (tasks.get(i).getId().equals(task.getId())) {
                tasks.set(i, task);
                flag = false;
                notifyItemChanged(i);
                break;
            }
        }
        if (flag) {
            tasks.add(task);
            tasks.sort(comparator);
            notifyItemInserted(tasks.indexOf(task));
        }
    }

    public void onRemoved(Task task) {
        for (int i = tasks.size() - 1; i >= 0; i--) {
            if (tasks.get(i).getId().equals(task.getId())) {
                tasks.remove(i);
                notifyItemRemoved(i);
                break;
            }
        }
    }

    public void showTasksByTag(String tag) {
        List<Task> newTasks;
        if (ALL.equals(tag)) newTasks = new ArrayList<>(TaskRepository.getInstance().getAllTasks());
        else if (NO.equals(tag)) newTasks = TaskRepository.getInstance().getTasksByTag(null);
        else newTasks = TaskRepository.getInstance().getTasksByTag(tag);

        if (newTasks == null || newTasks.size() == 0) {
            int size = tasks.size();
            tasks.clear();
            notifyItemRangeRemoved(0, size);
            return;
        }

        newTasks.sort(comparator);

        for (int i = tasks.size() - 1; i >= 0; i--) {
            Task task = tasks.get(i);
            boolean flag = true;
            for (Task newTask : newTasks) {
                if (task.getId().equals(newTask.getId())) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                tasks.remove(i);
                notifyItemRemoved(i);
            }
        }

        for (int i = 0; i < newTasks.size(); i++) {
            Task newTask = newTasks.get(i);
            boolean flag = true;
            for (Task task : tasks) {
                if (task.getId().equals(newTask.getId())) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                tasks.add(i, newTask);
                notifyItemInserted(i);
            }
        }
    }

    public void setCheck(boolean check) {
        isCheck = check;
    }

    public void selectAll() {
        if (selectTasks.size() == tasks.size()) {
            selectTasks.clear();
            notifyItemRangeChanged(0, tasks.size());
            return;
        }
        selectTasks.clear();
        for (Task task : tasks) {
            selectTasks.put(task.getId(), task);
        }
        notifyItemRangeChanged(0, tasks.size());
    }

    public void unSelectAll() {
        for (int i = 0; i < tasks.size(); i++) {
            Task task = tasks.get(i);
            if (selectTasks.containsKey(task.getId())) {
                selectTasks.remove(task.getId());
                notifyItemChanged(i);
            }
        }
        selectTasks.clear();
    }

    public List<Task> getSelectTasks() {
        return new ArrayList<>(selectTasks.values());
    }

    public void deleteSelectTasks() {
        for (int i = tasks.size() - 1; i >= 0; i--) {
            Task task = tasks.get(i);
            if (selectTasks.containsKey(task.getId())) {
                tasks.remove(i);
                TaskRepository.getInstance().deleteTask(task);
                notifyItemRemoved(i);
            }
        }
        selectTasks.clear();
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewTaskItemBinding binding;
        private final Context context;

        public ViewHolder(ViewTaskItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            binding.getRoot().setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                if (isCheck) {
                    if (selectTasks.containsKey(task.getId())) {
                        selectTasks.remove(task.getId());
                    } else {
                        selectTasks.put(task.getId(), task);
                    }
                    notifyItemChanged(index);
                } else {
                    NavController controller = Navigation.findNavController(MainApplication.getActivity(), R.id.con_view);
                    controller.navigate(TaskViewDirections.actionTaskToTaskInfo(task));
                }
            });

            binding.getRoot().setOnLongClickListener(v -> {
                if (!isCheck) {
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    selectTasks.put(task.getId(), task);
                    notifyItemChanged(index);
                    parent.showBottomBar();
                }
                return true;
            });
        }

        public void refreshItem(Task task) {
            binding.icon.setImageResource(task.getType().getTypeResource());
            binding.taskName.setText(task.getTitle());
            binding.taskDes.setText(task.getDescription(context));
            TaskConfig config = TaskRepository.getInstance().getTaskConfig(task.getId());
            binding.timeText.setText(AppUtils.formatDateLocalDate(context, sortType.ordinal() < 2 ? config.getCreateTime() : config.getModifyTime()));
            binding.getRoot().setChecked(selectTasks.containsKey(task.getId()));
            binding.taskTag.setText(config.getTag());

            binding.getRoot().setCardBackgroundColor(DisplayUtils.getAttrColor(context, task.isAcrossAppTask() ? com.google.android.material.R.attr.colorSecondaryContainer : com.google.android.material.R.attr.colorSurfaceVariant, 0));
        }
    }
}