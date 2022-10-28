package top.bogey.touch_tool.ui.tasks;

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
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.datepicker.CalendarConstraints;
import com.google.android.material.datepicker.DateValidatorPointForward;
import com.google.android.material.datepicker.MaterialDatePicker;
import com.google.android.material.timepicker.MaterialTimePicker;
import com.google.android.material.timepicker.TimeFormat;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewTasksItemBinding;
import top.bogey.touch_tool.room.bean.Action;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.room.data.TaskRepository;
import top.bogey.touch_tool.ui.actions.ActionFloatView;
import top.bogey.touch_tool.ui.record.RecordFloatView;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;

@SuppressLint("SetTextI18n")
public class TasksRecyclerViewAdapter extends RecyclerView.Adapter<TasksRecyclerViewAdapter.ViewHolder> {
    private final MainViewModel viewModel;
    private final List<Task> tasks = new ArrayList<>();

    public TasksRecyclerViewAdapter() {
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

    public void setTasks(List<Task> newTasks) {
        if (newTasks == null) {
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
            if (flag) {
                tasks.remove(i);
                notifyItemRemoved(i);
            }
        }

        // 查找新增的
        for (Task newTask : newTasks) {
            boolean flag = true;
            for (Task task : tasks) {
                if (task.getId().equals(newTask.getId())) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                tasks.add(newTask);
                notifyItemInserted(tasks.size() - 1);
            }
        }
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewTasksItemBinding binding;
        private final Context context;
        private final ActionsRecyclerViewAdapter adapter;
        private boolean isDeleteMode = false;

        @SuppressLint({"NonConstantResourceId", "PrivateResource"})
        public ViewHolder(ViewTasksItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            adapter = new ActionsRecyclerViewAdapter();
            binding.actionBox.setAdapter(adapter);

            binding.titleEdit.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)) {
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    Editable text = binding.titleEdit.getText();
                    if (text != null && text.length() > 0) {
                        task.setTitle(text.toString());
                        notifyItemChanged(index);
                        TaskRepository.getInstance(context).saveTask(task, true);
                    }
                    binding.titleEdit.setText(task.getTitle());
                    itemView.requestFocus();
                }
                return true;
            });

            binding.statusGroup.addOnButtonCheckedListener((group, checkedId, isChecked) -> {
                if (isChecked) {
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    TaskStatus status = task.getStatus();
                    switch (checkedId) {
                        case R.id.close_button:
                            task.setStatus(TaskStatus.CLOSED);
                            break;
                        case R.id.auto_button:
                            task.setStatus(TaskStatus.AUTO);
                            break;
                        case R.id.manual_button:
                            task.setStatus(TaskStatus.MANUAL);
                            break;
                        case R.id.time_button:
                            task.setStatus(TaskStatus.TIME);
                            break;
                    }
                    refreshItem(task);
                    if (status != task.getStatus()) {
                        TaskRepository.getInstance(context).saveTask(task, true);
                    }
                }
            });

            binding.calendarButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);

                CalendarConstraints calendarConstraints = new CalendarConstraints.Builder()
                        .setValidator(DateValidatorPointForward.now())
                        .build();

                MaterialDatePicker<Long> picker = MaterialDatePicker.Builder
                        .datePicker()
                        .setSelection(task.getTime())
                        .setInputMode(MaterialDatePicker.INPUT_MODE_CALENDAR)
                        .setCalendarConstraints(calendarConstraints)
                        .build();

                picker.show(MainApplication.getActivity().getSupportFragmentManager(), null);

                picker.addOnPositiveButtonClickListener(selection -> {
                    task.setTime(AppUtils.mergeDateTime(selection, task.getTime()));
                    refreshTime(task);
                    TaskRepository.getInstance(context).saveTask(task, true);
                });
            });

            binding.dateButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                Calendar calendar = Calendar.getInstance();
                calendar.setTimeInMillis(task.getTime());

                MaterialTimePicker picker = new MaterialTimePicker.Builder()
                        .setInputMode(MaterialTimePicker.INPUT_MODE_CLOCK)
                        .setTimeFormat(TimeFormat.CLOCK_24H)
                        .setHour(calendar.get(Calendar.HOUR_OF_DAY))
                        .setMinute(calendar.get(Calendar.MINUTE))
                        .build();

                picker.show(MainApplication.getActivity().getSupportFragmentManager(), null);

                picker.addOnPositiveButtonClickListener(view -> {
                    calendar.set(Calendar.HOUR_OF_DAY, picker.getHour());
                    calendar.set(Calendar.MINUTE, picker.getMinute());
                    calendar.set(Calendar.SECOND, 0);
                    task.setTime(calendar.getTimeInMillis());
                    refreshTime(task);
                    TaskRepository.getInstance(context).saveTask(task, true);
                });
            });

            binding.everyDayButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                int periodic = task.getPeriodic();

                MaterialTimePicker picker = new MaterialTimePicker.Builder()
                        .setInputMode(MaterialTimePicker.INPUT_MODE_CLOCK)
                        .setTimeFormat(TimeFormat.CLOCK_24H)
                        .setHour(periodic / 60)
                        .setMinute(periodic % 60)
                        .setTitleText(R.string.task_select_time_tips)
                        .build();

                picker.show(MainApplication.getActivity().getSupportFragmentManager(), null);

                picker.addOnPositiveButtonClickListener(view -> {
                    task.setPeriodic(picker.getHour() * 60 + picker.getMinute());
                    if (picker.getHour() == 0 && picker.getMinute() == 0) task.setPeriodic(24 * 60);
                    if (task.getPeriodic() < 15) task.setPeriodic(0);
                    refreshTime(task);
                    TaskRepository.getInstance(context).saveTask(task, true);
                });
            });

            binding.deleteButton.setOnClickListener(v -> {
                if (isDeleteMode) {
                    int index = getBindingAdapterPosition();
                    Task task = tasks.get(index);
                    tasks.remove(index);
                    notifyItemRemoved(index);
                    TaskRepository.getInstance(context).deleteTask(task);
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
                    TaskRepository.getInstance(context).saveTask(task);
                }).show();
            });

            binding.addButton.setOnLongClickListener(v -> {
                int index = getBindingAdapterPosition();
                Task task = tasks.get(index);
                new RecordFloatView(context, task, result -> {
                    TaskRepository.getInstance(context).saveTask(task);
                    notifyItemChanged(index);
                }).show();
                return true;
            });
        }

        public void refreshItem(Task task) {
            View child = binding.statusGroup.getChildAt(task.getStatus().ordinal());
            binding.statusGroup.check(child.getId());
            boolean isCommon = task.getPkgName().equals(context.getString(R.string.common_package_name));
            binding.timeButton.setVisibility(isCommon ? View.VISIBLE : View.GONE);
            binding.titleEdit.setText(task.getTitle());
            adapter.setTask(task);

            binding.timeBox.setVisibility(View.GONE);
            String hint = "";
            switch (task.getStatus()) {
                case CLOSED:
                    hint = context.getString(R.string.run_close);
                    break;
                case AUTO:
                    hint = context.getString(R.string.run_auto);
                    break;
                case MANUAL:
                    hint = context.getString(R.string.run_manual);
                    break;
                case TIME:
                    hint = context.getString(R.string.run_time);
                    binding.timeBox.setVisibility(View.VISIBLE);
                    refreshTime(task);
                    break;
            }
            binding.textInputLayout.setHint(hint);
            binding.getRoot().setCardBackgroundColor(DisplayUtils.getAttrColor(context, (!task.isAcrossApp() && task.getStatus() != TaskStatus.TIME) ? com.google.android.material.R.attr.colorSurfaceVariant : com.google.android.material.R.attr.colorSecondaryContainer, 0));
        }

        public void refreshTime(Task task) {
            String startTime = context.getString(R.string.task_start_time, AppUtils.formatDateMinute(task.getTime()));
            String periodicTime = "";
            if (task.getPeriodic() > 0) periodicTime = context.getString(R.string.task_periodic_time, task.getPeriodic() / 60, task.getPeriodic() % 60);
            binding.dateText.setText(startTime + periodicTime);
        }
    }
}