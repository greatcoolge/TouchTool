package top.bogey.auto_touch.ui.tasks;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.text.Editable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButtonToggleGroup;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.google.android.material.textview.MaterialTextView;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.MainViewModel;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.ViewTasksItemBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.room.bean.TaskTime;
import top.bogey.auto_touch.ui.actions.ActionFloatView;
import top.bogey.auto_touch.ui.record.RecordFloatView;
import top.bogey.auto_touch.utils.DisplayUtils;

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
        private final LinearLayout timeBox;
        private final MaterialTextView dayText;
        private boolean isDeleteMode = false;
        private final Context context;

        @SuppressLint({"NonConstantResourceId", "PrivateResource"})
        public ViewHolder(ViewTasksItemBinding binding) {
            super(binding.getRoot());
            context = itemView.getContext();
            group = binding.statusGroup;
            titleEdit = binding.titleEdit;
            textInputLayout = binding.textInputLayout;
            timeBox = binding.timeBox;
            dayText = binding.dayText;

            adapter = new ActionsRecyclerViewAdapter();
            binding.actionBox.setAdapter(adapter);

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
                        case R.id.alarm_button:
                            task.setStatus(TaskStatus.ALARM);
                            break;
                    }
                    refreshItem(task);
                    if (status != task.getStatus()){
                        viewModel.saveTask(task);
                    }
                }
            });

            binding.deleteButton.setOnClickListener(v -> {
                if (isDeleteMode){
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    tasks.remove(index);
                    notifyItemRemoved(index);
                    viewModel.deleteTask(task);
                } else {
                    isDeleteMode = true;
                    binding.deleteButton.setIconTint(ColorStateList.valueOf(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorError, 0)));
                    binding.deleteButton.setBackgroundTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorErrorContainer, 0)));
                    binding.deleteButton.postDelayed(() -> {
                        binding.deleteButton.setIconTintResource(com.google.android.material.R.color.m3_text_button_foreground_color_selector);
                        binding.deleteButton.setBackgroundTintList(ColorStateList.valueOf(context.getResources().getColor(android.R.color.transparent, null)));
                        isDeleteMode = false;
                    }, 3000);
                }
            });

            binding.shareButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                viewModel.setCopyTask(task);
            });

            binding.addButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                Action action = new Action();
                new ActionFloatView(context, task, action, result -> {
                    task.getActions().add(action);
                    adapter.notifyNew();
                    viewModel.saveTask(task);
                }).show();
            });

            binding.addButton.setOnLongClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                new RecordFloatView(context, task, result -> notifyItemChanged(index)).show();
                return true;
            });

            binding.dayEditButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);

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
                    hint = context.getString(R.string.run_close);
                    break;
                case AUTO:
                    hint = context.getString(R.string.run_auto);
                    break;
                case MANUAL:
                    hint = context.getString(R.string.run_manual);
                    break;
                case ALARM:
                    hint = context.getString(R.string.run_alarm);
                    break;
            }
            textInputLayout.setHint(hint);
            timeBox.setVisibility(task.getStatus() == TaskStatus.ALARM ? View.VISIBLE : View.GONE);
            setDate(task.getTime());
        }

        @SuppressLint("SetTextI18n")
        private void setDate(TaskTime time){
            Calendar calendar = Calendar.getInstance();
            calendar.setTimeInMillis(time.getTime());
            int week = calendar.get(Calendar.DAY_OF_WEEK);
            week = (week + 5) % 7 + 1;
            String dateString = context.getString(R.string.start_date,
                    calendar.get(Calendar.YEAR),
                    calendar.get(Calendar.MONTH) + 1,
                    calendar.get(Calendar.DAY_OF_MONTH),
                    week,
                    calendar.get(Calendar.HOUR_OF_DAY),
                    calendar.get(Calendar.MINUTE)
            );
            String intervalTitle = time.getIntervalTitle(context);
            if (intervalTitle == null || intervalTitle.isEmpty()){
                dayText.setText(dateString);
            } else {
                dayText.setText(dateString + "\n" + intervalTitle);
            }
        }
    }
}