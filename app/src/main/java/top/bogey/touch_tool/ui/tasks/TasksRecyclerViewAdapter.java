package top.bogey.touch_tool.ui.tasks;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.mmkv.MMKV;

import java.util.Comparator;
import java.util.List;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskConfig;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTasksItemBinding;
import top.bogey.touch_tool.utils.AppUtils;

public class TasksRecyclerViewAdapter extends RecyclerView.Adapter<TasksRecyclerViewAdapter.ViewHolder> {
    private final static String SORT_TYPE = "sort_type";
    private final List<Task> tasks;

    private final TaskRepository repository = TaskRepository.getInstance();
    private ShotType sortType = MMKV.defaultMMKV().decodeParcelable(SORT_TYPE, ShotType.class, ShotType.CREATE_TIME_ASC);
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

    public TasksRecyclerViewAdapter() {
        tasks = TaskRepository.getInstance().getAllTasks(null);
    }

    public void setSortType(ShotType type) {
        if (type == sortType) type = type == ShotType.CREATE_TIME_ASC ? ShotType.CREATE_TIME_DESC : ShotType.MODIFY_TIME_DESC;
        else type = type == ShotType.CREATE_TIME_ASC ? ShotType.CREATE_TIME_ASC : ShotType.MODIFY_TIME_ASC;

        sortType = type;
        tasks.sort(comparator);
        notifyItemRangeChanged(0, tasks.size());
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewTasksItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, int position) {
        holder.refreshItem(tasks.get(position));
    }

    @Override
    public int getItemCount() {
        return tasks.size();
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

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewTasksItemBinding binding;
        private final Context context;

        public ViewHolder(ViewTasksItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            binding.getRoot().setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                NavController controller = Navigation.findNavController(MainApplication.getActivity(), R.id.con_view);
                controller.navigate(TasksViewDirections.actionTasksToTaskInfo(tasks.get(index)));
            });
        }

        public void refreshItem(Task task) {
            binding.icon.setImageResource(task.getType().getTypeResource());
            binding.taskName.setText(task.getTitle());
            binding.taskDes.setText(task.getDescription(context));
            TaskConfig config = TaskRepository.getInstance().getTaskConfig(task.getId());
            binding.timeText.setText(AppUtils.formatDateLocalDate(context, sortType.ordinal() < 2 ? config.getCreateTime() : config.getModifyTime()));
        }
    }
}