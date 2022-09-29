package top.bogey.touch_tool.ui.apps;

import android.content.Context;
import android.content.pm.PackageManager;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.checkbox.MaterialCheckBox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewTasksExportAppItemBinding;
import top.bogey.touch_tool.databinding.ViewTasksExportTaskItemBinding;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.node.TaskNode;
import top.bogey.touch_tool.room.data.TaskRepository;

public class TasksExportRecyclerViewAdapter extends RecyclerView.Adapter<TasksExportRecyclerViewAdapter.ViewHolder> {
    private static final int APP = 1;
    private static final int TASK = 2;

    private final TasksExportView parent;
    private final MainViewModel viewModel;
    private final List<Task> tasks;

    private List<Object> showData = new ArrayList<>();
    public final List<Task> selectTasks = new ArrayList<>();
    private final Map<String, Boolean> showInfo = new HashMap<>();

    public TasksExportRecyclerViewAdapter(TasksExportView parent) {
        this.parent = parent;
        viewModel = new ViewModelProvider(parent.requireActivity()).get(MainViewModel.class);
        tasks = TaskRepository.getInstance(parent.getContext()).getAllTasks();
        selectTasks.addAll(tasks);

        List<TaskNode.TaskGroup> originData = TaskRepository.getInstance(parent.getContext()).getTaskGroups();
        for (TaskNode.TaskGroup taskGroup : originData) {
            showData.add(taskGroup);
            showInfo.put(taskGroup.getPkgName(), Boolean.FALSE);
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        if (viewType == APP) {
            return new ViewHolder(ViewTasksExportAppItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
        }
        return new ViewHolder(ViewTasksExportTaskItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.refreshView(showData.get(position), getItemViewType(position));
    }

    @Override
    public int getItemCount() {
        return showData.size();
    }

    @Override
    public int getItemViewType(int position) {
        return showData.get(position) instanceof Task ? TASK : APP;
    }

    private void refreshShowData(){
        List<Object> tmp = new ArrayList<>();

        for (Object data : showData) {
            if (data instanceof TaskNode.TaskGroup){
                TaskNode.TaskGroup taskGroup = (TaskNode.TaskGroup) data;
                tmp.add(taskGroup);
                if (Boolean.TRUE.equals(showInfo.get(taskGroup.getPkgName())))
                    tmp.addAll(getTasksByPackageName(taskGroup.getPkgName()));
            }
        }

        showData = tmp;
    }

    private List<Task> getTasksByPackageName(String pkgName){
        List<Task> tasks = new ArrayList<>();
        for (Task task : this.tasks) {
            if (task.getPkgName().equals(pkgName))
                tasks.add(task);
        }
        return tasks;
    }

    private int getSelectCountByPackageName(String pkgName){
        int count = 0;
        for (Task task : selectTasks) {
            if (task.getPkgName().equals(pkgName))
                count++;
        }
        return count;
    }

    private int getLastTaskGroupIndex(int index){
        for (int i = index; i > 0; i--) {
            if (APP == getItemViewType(i)){
                return i;
            }
        }
        return 0;
    }

    private void refreshSelectAllBox(){
        if (selectTasks.size() == 0) parent.refreshSelectAllBox(MaterialCheckBox.STATE_UNCHECKED);
        else if (selectTasks.size() == tasks.size()) parent.refreshSelectAllBox(MaterialCheckBox.STATE_CHECKED);
        else parent.refreshSelectAllBox(MaterialCheckBox.STATE_INDETERMINATE);
    }

    public void selectAll(boolean select){
        selectTasks.clear();
        if (select){
            selectTasks.addAll(tasks);
        }
        notifyDataSetChanged();
    }

    public void selectSomeByPackageName(String pkgName){
        List<Task> tasks = getTasksByPackageName(pkgName);
        if (getSelectCountByPackageName(pkgName) == tasks.size()){
            selectTasks.removeIf(task -> task.getPkgName().equals(pkgName));
        } else {
            for (Task task : tasks) {
                if (!selectTasks.contains(task)) selectTasks.add(task);
            }
        }
        notifyDataSetChanged();
    }

    protected class ViewHolder extends RecyclerView.ViewHolder{
        private ViewTasksExportAppItemBinding appBinding;
        private ViewTasksExportTaskItemBinding taskBinding;
        private final Context context;

        public ViewHolder(ViewTasksExportAppItemBinding binding) {
            super(binding.getRoot());
            appBinding = binding;
            context = binding.getRoot().getContext();
            binding.getRoot().setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                TaskNode.TaskGroup taskGroup = (TaskNode.TaskGroup) showData.get(index);
                int size = getTasksByPackageName(taskGroup.getPkgName()).size();
                if (Boolean.FALSE.equals(showInfo.get(taskGroup.getPkgName()))){
                    showInfo.put(taskGroup.getPkgName(), Boolean.TRUE);
                    refreshShowData();
                    notifyItemRangeInserted(index + 1, size);
                } else {
                    showInfo.put(taskGroup.getPkgName(), Boolean.FALSE);
                    refreshShowData();
                    notifyItemRangeRemoved(index + 1, size);
                }
            });

            binding.numberText.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                TaskNode.TaskGroup taskGroup = (TaskNode.TaskGroup) showData.get(index);
                selectSomeByPackageName(taskGroup.getPkgName());
                refreshSelectAllBox();
            });
        }

        public ViewHolder(ViewTasksExportTaskItemBinding binding){
            super(binding.getRoot());
            taskBinding = binding;
            context = binding.getRoot().getContext();
            binding.checkBox.addOnCheckedStateChangedListener((checkBox, state) -> {
                int index = getBindingAdapterPosition();
                Task task = (Task) showData.get(index);
                if (state == MaterialCheckBox.STATE_CHECKED) {
                    if (!selectTasks.contains(task)){
                        selectTasks.add(task);
                        refreshSelectAllBox();
                    }
                } else {
                    selectTasks.remove(task);
                    refreshSelectAllBox();
                }
            });

            binding.getRoot().setOnClickListener(v -> {
                binding.checkBox.setChecked(!binding.checkBox.isChecked());
                int index = getBindingAdapterPosition();
                notifyItemChanged(getLastTaskGroupIndex(index));
            });
        }

        public void refreshView(Object data, int viewType){
            switch (viewType) {
                case APP:
                    TaskNode.TaskGroup taskGroup = (TaskNode.TaskGroup) data;
                    AppInfo appInfo = viewModel.getAppInfoByPkgName(taskGroup.getPkgName());
                    appBinding.appName.setText(appInfo.appName);
                    PackageManager manager = context.getPackageManager();
                    if (appInfo.packageName.equals(context.getString(R.string.common_package_name))){
                        appBinding.icon.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
                    } else {
                        appBinding.icon.setImageDrawable(appInfo.info.applicationInfo.loadIcon(manager));
                    }
                    String string = context.getString(R.string.export_tasks_count, getSelectCountByPackageName(taskGroup.getPkgName()), taskGroup.getCount());
                    appBinding.numberText.setText(string);
                    break;
                case TASK:
                    Task task = (Task) data;
                    taskBinding.appName.setText(task.getTitle());
                    taskBinding.checkBox.setChecked(selectTasks.contains(task));
                    break;
            }
        }
    }
}
