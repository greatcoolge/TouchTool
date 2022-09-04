package top.bogey.touch_tool.ui.tasks;

import android.annotation.SuppressLint;
import android.content.res.ColorStateList;
import android.text.Editable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.button.MaterialButtonToggleGroup;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewTasksItemBinding;
import top.bogey.touch_tool.room.bean.Action;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.ui.actions.ActionFloatView;
import top.bogey.touch_tool.ui.record.RecordFloatView;
import top.bogey.touch_tool.utils.DisplayUtils;

public class TasksRecyclerViewAdapter extends RecyclerView.Adapter<TasksRecyclerViewAdapter.ViewHolder> {
    private final MainViewModel viewModel;
    private final List<Task> tasks = new ArrayList<>();

    public TasksRecyclerViewAdapter(){
        viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
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

    public void setTasks(List<Task> newTasks){
        if (newTasks == null){
            int size = tasks.size();
            tasks.clear();
            notifyItemRangeRemoved(0, size);
            return;
        }
        // 查找删除的 或 动作变更了的
        for (int i = tasks.size() - 1; i >= 0; i--) {
            Task task = tasks.get(i);
            boolean flag = true;
            for (Task newTask : newTasks) {
                if (task.getId().equals(newTask.getId())) {
                    flag = false;
                    break;
                }
            }
            if (flag){
                tasks.remove(i);
                notifyItemRemoved(i);
            }
        }

        // 查找新增的
        for (Task newTask : newTasks) {
            boolean flag = true;
            for (Task task : tasks) {
                if (task.getId().equals(newTask.getId())){
                    flag = false;
                    break;
                }
            }
            if (flag){
                tasks.add(newTask);
                notifyItemInserted(tasks.size() - 1);
            }
        }
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final MaterialButtonToggleGroup group;
        private final TextInputEditText titleEdit;
        private final ActionsRecyclerViewAdapter adapter;
        private final TextInputLayout textInputLayout;
        private boolean isDeleteMode = false;

        @SuppressLint({"NonConstantResourceId", "PrivateResource"})
        public ViewHolder(ViewTasksItemBinding binding) {
            super(binding.getRoot());
            group = binding.statusGroup;
            titleEdit = binding.titleEdit;
            textInputLayout = binding.textInputLayout;
            MaterialButton add = binding.addButton;
            MaterialButton delete = binding.deleteButton;
            MaterialButton copy = binding.shareButton;
            RecyclerView actionBox = binding.actionBox;

            adapter = new ActionsRecyclerViewAdapter();
            actionBox.setAdapter(adapter);

            titleEdit.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)){
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    Editable text = titleEdit.getText();
                    if (text != null && text.length() > 0){
                        task.setTitle(text.toString());
                        notifyItemChanged(index);
                        viewModel.saveTask(task);
                    }
                    titleEdit.setText(task.getTitle());
                    itemView.requestFocus();
                }
                return true;
            });

            group.addOnButtonCheckedListener((group, checkedId, isChecked) -> {
                if (isChecked){
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    TaskStatus status = task.getStatus();
                    switch (checkedId){
                        case R.id.close_button:
                            task.setStatus(TaskStatus.CLOSED);
                            break;
                        case R.id.auto_button:
                            task.setStatus(TaskStatus.AUTO);
                            break;
                        case R.id.manual_button:
                            task.setStatus(TaskStatus.MANUAL);
                            break;
                    }
                    refreshItem(task);
                    if (status != task.getStatus()){
                        viewModel.saveTask(task);
                    }
                }
            });

            delete.setOnClickListener(v -> {
                if (isDeleteMode){
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    tasks.remove(index);
                    notifyItemRemoved(index);
                    viewModel.deleteTask(task);
                } else {
                    isDeleteMode = true;
                    delete.setIconTint(ColorStateList.valueOf(DisplayUtils.getAttrColor(itemView.getContext(), com.google.android.material.R.attr.colorError, 0)));
                    delete.setBackgroundTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(itemView.getContext(), com.google.android.material.R.attr.colorErrorContainer, 0)));
                    delete.postDelayed(() -> {
                        delete.setIconTintResource(com.google.android.material.R.color.m3_text_button_foreground_color_selector);
                        delete.setBackgroundTintList(ColorStateList.valueOf(itemView.getContext().getResources().getColor(android.R.color.transparent, null)));
                        isDeleteMode = false;
                    }, 3000);
                }
            });

            copy.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                viewModel.setCopyTask(task);
            });

            add.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                Action action = new Action();
                new ActionFloatView(itemView.getContext(), task, action, result -> {
                    task.getActions().add(action);
                    adapter.notifyNew();
                    viewModel.saveTask(task);
                }).show();
            });

            add.setOnLongClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                new RecordFloatView(itemView.getContext(), task, result -> notifyItemChanged(index)).show();
                return true;
            });
        }

        public void refreshItem(Task task){
            View child = group.getChildAt(task.getStatus().ordinal());
            group.check(child.getId());
            titleEdit.setText(task.getTitle());
            adapter.setTask(task);
            String hint = "";
            switch (task.getStatus()){
                case CLOSED:
                    hint = itemView.getContext().getString(R.string.run_close);
                    break;
                case AUTO:
                    hint = itemView.getContext().getString(R.string.run_auto);
                    break;
                case MANUAL:
                    hint = itemView.getContext().getString(R.string.run_manual);
                    break;
            }
            textInputLayout.setHint(hint);
        }
    }
}