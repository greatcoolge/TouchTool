package top.bogey.auto_touch.ui.tasks;

import android.annotation.SuppressLint;
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

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.MainViewModel;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.ViewTasksItemBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.actions.ActionFloatView;
import top.bogey.auto_touch.ui.record.RecordFloatView;
import top.bogey.auto_touch.utils.AppUtils;
import top.bogey.auto_touch.utils.SelectCallback;

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

        @SuppressLint({"NonConstantResourceId", "PrivateResource"})
        public ViewHolder(ViewTasksItemBinding binding) {
            super(binding.getRoot());
            group = binding.statusGroup;
            titleEdit = binding.titleEdit;
            MaterialButton add = binding.addButton;
            MaterialButton delete = binding.deleteButton;
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
                for (int i = 0; i < group.getChildCount(); i++) {
                    MaterialButton button = (MaterialButton) group.getChildAt(i);
                    button.setStrokeWidth((int) itemView.getContext().getResources().getDimension(com.google.android.material.R.dimen.m3_btn_stroke_size));
                }

                if (isChecked){
                    MaterialButton checkedButton = group.findViewById(checkedId);
                    checkedButton.setStrokeWidth(0);
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
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
                    viewModel.saveTask(task);
                }
            });

            delete.setOnClickListener(v -> AppUtils.showDialog(itemView.getContext(), R.string.delete_task_tips, new SelectCallback() {
                @Override
                public void onEnter() {
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    tasks.remove(index);
                    notifyItemRemoved(index);
                    viewModel.deleteTask(task);
                }
            }));

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
        }
    }
}