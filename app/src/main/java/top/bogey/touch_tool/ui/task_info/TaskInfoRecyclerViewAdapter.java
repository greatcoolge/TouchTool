package top.bogey.touch_tool.ui.task_info;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.text.Editable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.ItemTouchHelper;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.TaskAction;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTaskInfoItemBinding;
import top.bogey.touch_tool.ui.behavior.BehaviorFloatView;
import top.bogey.touch_tool.ui.behavior.BehaviorRecyclerViewAdapter;
import top.bogey.touch_tool.ui.record.RecordFloatView;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;

public class TaskInfoRecyclerViewAdapter extends RecyclerView.Adapter<TaskInfoRecyclerViewAdapter.ViewHolder> {
    private final MainViewModel viewModel;
    private final Task baseTask;
    private final List<Task> tasks = new ArrayList<>();
    private final TaskInfoView parent;
    private boolean isExchange = false;

    public TaskInfoRecyclerViewAdapter(TaskInfoView parent, Task task) {
        this.parent = parent;
        baseTask = task;
        tasks.add(task);
        if (task.getSubTasks() != null) tasks.addAll(task.getSubTasks());
        viewModel = MainViewModel.getInstance();
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewTaskInfoItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, int position) {
        holder.refreshItem(tasks.get(position));
    }

    @Override
    public int getItemCount() {
        return tasks.size();
    }

    public void onTaskChanged(Task task) {
        if (!tasks.contains(task)) {
            tasks.add(task);
            notifyItemInserted(tasks.size() - 1);
        } else if (tasks.indexOf(task) == 0 && isExchange) {
            notifyItemRangeChanged(0, tasks.size());
            isExchange = false;
        }
    }

    public void onTaskActionChanged(TaskAction taskAction) {
        Task subTask = baseTask.getSubTaskById(taskAction.getId());
        if (subTask == null) return;
        int index = baseTask.getSubTasks().indexOf(subTask);
        notifyItemChanged(index + 1);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewTaskInfoItemBinding binding;
        private final Context context;

        private final BehaviorRecyclerViewAdapter adapter;
        private boolean isDeleteMode = false;

        @SuppressLint({"NonConstantResourceId", "PrivateResource"})
        public ViewHolder(ViewTaskInfoItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            viewModel.copyBehavior.observe(parent.getViewLifecycleOwner(), behavior -> binding.pasteButton.setVisibility(behavior == null ? View.GONE : View.VISIBLE));

            adapter = new BehaviorRecyclerViewAdapter(parent, baseTask);
            ItemTouchHelper helper = new ItemTouchHelper(new BehaviorRecyclerViewAdapter.BehaviorHelperCallback(adapter));
            binding.behaviorBox.setAdapter(adapter);
            helper.attachToRecyclerView(binding.behaviorBox);

            binding.titleEdit.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)) {
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    Editable text = binding.titleEdit.getText();
                    if (text != null && text.length() > 0) {
                        task.setTitle(text.toString().trim());
                        notifyItemChanged(index);
                        TaskRepository.getInstance().saveTask(baseTask);
                    }
                    binding.titleEdit.setText(task.getTitle());
                    itemView.requestFocus();
                }
                return true;
            });

            binding.deleteButton.setOnClickListener(v -> {
                if (isDeleteMode) {
                    int index = getBindingAdapterPosition();
                    Task task = tasks.remove(index);
                    notifyItemRemoved(index);
                    if (index == 0) TaskRepository.getInstance().deleteTask(baseTask);
                    else {
                        baseTask.removeSubTask(task);
                        TaskRepository.getInstance().saveTask(baseTask);
                    }
                } else {
                    isDeleteMode = true;
                    binding.deleteButton.setIconTint(ColorStateList.valueOf(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorError, 0)));
                    binding.deleteButton.setBackgroundTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorErrorContainer, 0)));
                    binding.deleteButton.postDelayed(() -> {
                        binding.deleteButton.setIconTint(ColorStateList.valueOf(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorPrimary, 0)));
                        binding.deleteButton.setBackgroundTintList(ColorStateList.valueOf(context.getResources().getColor(android.R.color.transparent, null)));
                        isDeleteMode = false;
                    }, 3000);
                }
            });

            binding.copyButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                viewModel.setCopyTask(task);
            });

            binding.pasteButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                task.addBehavior(viewModel.getCopyBehavior());
                notifyItemChanged(index);
                TaskRepository.getInstance().saveTask(baseTask);
            });

            binding.pasteButton.setOnLongClickListener(v -> {
                viewModel.setCopyBehavior(null);
                return true;
            });

            binding.addButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                Behavior behavior = new Behavior();
                new BehaviorFloatView(context, baseTask, task, behavior, result -> adapter.addBehavior(behavior)).show();
            });

            binding.recordButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                new RecordFloatView(context, baseTask, tasks.get(index), result -> {
                    notifyItemChanged(index);
                    TaskRepository.getInstance().saveTask(baseTask);
                }).show();
            });

            binding.textInputLayout.setEndIconOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                if (index == 0) return;
                Task mainTask = tasks.get(0);
                Task task = tasks.get(index);

                Task tmp = AppUtils.copy(task);
                task.setBehaviors(mainTask.getBehaviors());
                task.setTitle(mainTask.getTitle());

                mainTask.setBehaviors(tmp.getBehaviors());
                mainTask.setTitle(tmp.getTitle());

                notifyItemChanged(0);
                notifyItemChanged(index);

                isExchange = true;
                TaskRepository.getInstance().saveTask(baseTask);
            });
        }

        public void refreshItem(Task task) {
            int index = getBindingAdapterPosition();
            adapter.setTask(task);
            binding.titleEdit.setText(task.getTitle());
            binding.getRoot().setCardBackgroundColor(DisplayUtils.getAttrColor(context, (index == 0) ? com.google.android.material.R.attr.colorSurfaceVariant : com.google.android.material.R.attr.colorOnSurfaceInverse, 0));
            if (index == 0) {
                binding.textInputLayout.setEndIconDrawable(R.drawable.icon_radio_checked);
            } else {
                if (baseTask.includeTaskAction(baseTask, task.getId())) {
                    binding.textInputLayout.setEndIconDrawable(R.drawable.icon_radio_selected);
                } else {
                    binding.textInputLayout.setEndIconDrawable(R.drawable.icon_radio_unchecked);
                }
            }
        }
    }
}