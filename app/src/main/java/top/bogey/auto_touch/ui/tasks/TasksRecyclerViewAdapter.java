package top.bogey.auto_touch.ui.tasks;

import android.content.Context;
import android.text.Editable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.RadioGroup;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentTasksItemBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.action.FloatActionEdit;
import top.bogey.auto_touch.ui.record.TaskRecordDialog;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class TasksRecyclerViewAdapter extends RecyclerView.Adapter<TasksRecyclerViewAdapter.ViewHolder> {
    private final TasksFragment parent;
    private final MainViewModel viewModel;
    private final List<Task> tasks = new ArrayList<>();

    public TasksRecyclerViewAdapter(TasksFragment parent){
        this.parent = parent;
        viewModel = new ViewModelProvider(parent.requireActivity()).get(MainViewModel.class);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FragmentTasksItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, int position) {
        Task task = tasks.get(position);
        View child = holder.group.getChildAt(task.getTaskStatus().ordinal());
        holder.group.check(child.getId());
        holder.titleEdit.setText(task.getTitle());
        holder.titleEdit.setTextColor(AppUtil.getGroupColor(parent.requireContext(), task.getGroupId()));
        holder.titleEdit.setInputType(EditorInfo.TYPE_NULL);
        holder.titleEdit.setBackground(null);
        holder.adapter.setTask(task);

        for (int i = 0; i < holder.groups.length; i++) {
            if (i == task.getGroupId() - 1){
                holder.groups[i].setAlpha(1f);
            } else {
                holder.groups[i].setAlpha(0.04f);
            }
        }
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
        public final ConstraintLayout layout;
        public final RadioGroup group;
        public final EditText titleEdit;
        public final Button add;
        public final Button delete;
        public final LinearLayout linearLayout;
        public final RecyclerView actionBox;
        public final Button[] groups = new Button[3];
        public ActionsRecyclerViewAdapter adapter;


        public ViewHolder(FragmentTasksItemBinding binding) {
            super(binding.getRoot());
            layout = binding.getRoot();
            group = binding.statusGroup;
            titleEdit = binding.titleEdit;
            add = binding.add;
            delete = binding.delete;
            linearLayout = binding.linearLayout;
            actionBox = binding.actionBox;
            groups[0] = binding.group1;
            groups[1] = binding.group2;
            groups[2] = binding.group3;

            adapter = new ActionsRecyclerViewAdapter(parent);
            actionBox.setAdapter(adapter);

            titleEdit.setOnLongClickListener(v -> {
                titleEdit.setInputType(EditorInfo.TYPE_CLASS_TEXT);
                titleEdit.setBackgroundResource(R.drawable.edit_text);
                titleEdit.requestFocus();
                InputMethodManager manager = (InputMethodManager) parent.requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
                if (manager != null) manager.showSoftInput(titleEdit, InputMethodManager.SHOW_IMPLICIT);
                return true;
            });

            titleEdit.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)){
                    Editable text = titleEdit.getText();
                    if (text != null && text.length() > 0){
                        int index = getAdapterPosition();
                        Task task = tasks.get(index);
                        task.setTitle(text.toString());
                        notifyItemChanged(index);
                        viewModel.saveTask(task);
                    }
                    titleEdit.setInputType(EditorInfo.TYPE_NULL);
                    titleEdit.setBackground(null);
                }
                return true;
            });

            group.setOnCheckedChangeListener((group, checkedId) -> {
                int selectIndex = group.indexOfChild(group.findViewById(checkedId));
                int index = getAdapterPosition();
                Task task = tasks.get(index);
                TaskStatus status = TaskStatus.values()[selectIndex];
                boolean b = task.getTaskStatus() == status;
                task.setTaskStatus(status);
                if (!b) viewModel.saveTask(task);
            });

            delete.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Task task = tasks.get(index);
                AppUtil.showSimpleDialog(parent.requireActivity(), parent.requireContext().getString(R.string.delete_task_tips, task.getTitle()), new SelectCallback() {
                    @Override
                    public void onEnter() {
                        tasks.remove(index);
                        notifyItemRemoved(index);
                        viewModel.deleteTask(task);
                    }

                    @Override
                    public void onCancel() { }
                });
            });

            add.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Task task = tasks.get(index);
                Action action = new Action();
                new FloatActionEdit(parent.requireContext(), task, action, () -> {
                    task.getActions().add(action);
                    adapter.notifyNew();
                    viewModel.saveTask(task);
                }).show();
            });

            add.setOnLongClickListener(v -> {
                int index = getAdapterPosition();
                Task task = tasks.get(index);
                new TaskRecordDialog(parent.requireContext(), task, () -> notifyItemChanged(index)).show();
                return true;
            });

            for (Button button : groups) {
                button.setOnClickListener(v -> {
                    int childIndex = linearLayout.indexOfChild((View) v.getParent());
                    int index = getAdapterPosition();
                    Task task = tasks.get(index);
                    if (task.getGroupId() == childIndex){
                        task.setGroupId(0);
                    } else {
                        task.setGroupId(childIndex);
                    }
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                });
            }
        }
    }
}