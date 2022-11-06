package top.bogey.touch_tool.ui.tasks;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.Editable;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.datepicker.CalendarConstraints;
import com.google.android.material.datepicker.DateValidatorPointForward;
import com.google.android.material.datepicker.MaterialDatePicker;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.imageview.ShapeableImageView;
import com.google.android.material.materialswitch.MaterialSwitch;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.timepicker.MaterialTimePicker;
import com.google.android.material.timepicker.TimeFormat;

import java.util.Calendar;
import java.util.LinkedHashMap;
import java.util.Map;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskType;
import top.bogey.touch_tool.database.bean.condition.NotificationCondition;
import top.bogey.touch_tool.database.bean.condition.TimeCondition;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTaskInfoBinding;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.TaskChangedCallback;

public class TaskInfoView extends Fragment implements TaskChangedCallback {
    private ViewTaskInfoBinding binding;
    private MainViewModel viewModel;
    private TaskInfoRecyclerViewAdapter adapter;
    private Task task;

    private TimeCondition timeCondition;
    private NotificationCondition notificationCondition;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = ViewTaskInfoBinding.inflate(inflater, container, false);
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);

        if (getArguments() != null) {
            task = getArguments().getParcelable("task");
        }

        adapter = new TaskInfoRecyclerViewAdapter(task);
        binding.tasksBox.setAdapter(adapter);

        ActionBar actionBar = ((AppCompatActivity) requireActivity()).getSupportActionBar();
        if (actionBar != null) {
            requireActivity().addMenuProvider(new MenuProvider() {
                @Override
                public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                    MenuItem menuItem = menu.add(R.string.across_app_task);
                    @SuppressLint("InflateParams") MaterialSwitch materialSwitch = (MaterialSwitch) inflater.inflate(R.layout.widget_switch, null);
                    menuItem.setActionView(materialSwitch);
                    menuItem.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
                    materialSwitch.setChecked(task.isAcrossApp());
                    materialSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
                        task.setAcrossApp(isChecked);
                        TaskRepository.getInstance().saveTask(task);
                        actionBar.setTitle(task.isAcrossApp() ? R.string.across_app_task : R.string.normal_task);
                    });
                    actionBar.setTitle(task.isAcrossApp() ? R.string.across_app_task : R.string.normal_task);
                }

                @Override
                public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                    return false;
                }
            }, getViewLifecycleOwner());
        }

        binding.statusGroup.addOnButtonCheckedListener((group, checkedId, isChecked) -> {
            if (isChecked) {
                TaskType taskType = task.getType();
                if (taskType == TaskType.IT_IS_TIME) timeCondition = (TimeCondition) task.getCondition();
                else if (taskType == TaskType.NEW_NOTIFICATION) notificationCondition = (NotificationCondition) task.getCondition();

                int index = group.indexOfChild(group.findViewById(checkedId));
                taskType = TaskType.values()[index];
                task.setType(taskType);
                if (taskType == TaskType.IT_IS_TIME) {
                    task.setCondition(timeCondition);
                    binding.dateText.setText(null);
                } else if (taskType == TaskType.NEW_NOTIFICATION) {
                    task.setCondition(notificationCondition);
                    binding.dateText.setText(null);
                } else task.setCondition(null);
                TaskRepository.getInstance().saveTask(task);
                refreshUI();
            }
        });
        View child = binding.statusGroup.getChildAt(task.getType().ordinal());
        binding.statusGroup.check(child.getId());

        binding.addButton.setOnClickListener(v -> {
            Task subTask = new Task(requireContext());
            task.addSubTask(subTask);
            adapter.taskChanged(subTask);
            TaskRepository.getInstance().saveTask(task);
        });

        binding.dateButton.setOnClickListener(v -> {
            TimeCondition condition;
            if (task.getCondition() == null || !(task.getCondition() instanceof TimeCondition)) {
                condition = new TimeCondition();
            } else {
                condition = (TimeCondition) task.getCondition();
            }

            CalendarConstraints calendarConstraints = new CalendarConstraints.Builder()
                    .setValidator(DateValidatorPointForward.now())
                    .build();

            MaterialDatePicker<Long> picker = MaterialDatePicker.Builder
                    .datePicker()
                    .setSelection(condition.getStartTime())
                    .setInputMode(MaterialDatePicker.INPUT_MODE_CALENDAR)
                    .setCalendarConstraints(calendarConstraints)
                    .build();

            picker.show(requireActivity().getSupportFragmentManager(), null);

            picker.addOnPositiveButtonClickListener(selection -> {
                condition.setStartTime(AppUtils.mergeDateTime(selection, condition.getStartTime()));
                task.setCondition(condition);
                TaskRepository.getInstance().saveTask(task);
                refreshTime();
            });
        });

        binding.timeButton.setOnClickListener(v -> {
            TimeCondition condition;
            if (task.getCondition() == null || !(task.getCondition() instanceof TimeCondition)) {
                condition = new TimeCondition();
            } else {
                condition = (TimeCondition) task.getCondition();
            }

            Calendar calendar = Calendar.getInstance();
            calendar.setTimeInMillis(condition.getStartTime());

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
                condition.setStartTime(calendar.getTimeInMillis());
                task.setCondition(condition);
                TaskRepository.getInstance().saveTask(task);
                refreshTime();
            });
        });

        binding.periodicButton.setOnClickListener(v -> {
            TimeCondition condition;
            if (task.getCondition() == null || !(task.getCondition() instanceof TimeCondition)) {
                condition = new TimeCondition();
            } else {
                condition = (TimeCondition) task.getCondition();
            }

            int periodic = condition.getPeriodic();
            MaterialTimePicker picker = new MaterialTimePicker.Builder()
                    .setInputMode(MaterialTimePicker.INPUT_MODE_CLOCK)
                    .setTimeFormat(TimeFormat.CLOCK_24H)
                    .setHour(periodic / 60)
                    .setMinute(periodic % 60)
                    .setTitleText(R.string.time_condition_periodic_tips)
                    .build();

            picker.show(MainApplication.getActivity().getSupportFragmentManager(), null);

            picker.addOnPositiveButtonClickListener(view -> {
                condition.setPeriodic(picker.getHour() * 60 + picker.getMinute());
                if (picker.getHour() == 0 && picker.getMinute() == 0) condition.setPeriodic(24 * 60);
                if (condition.getPeriodic() < 15) condition.setPeriodic(0);
                task.setCondition(condition);
                TaskRepository.getInstance().saveTask(task);
                refreshTime();
            });
        });

        binding.textButton.setOnClickListener(v -> {
            NotificationCondition condition;
            if (task.getCondition() == null || !(task.getCondition() instanceof NotificationCondition)) {
                condition = new NotificationCondition();
            } else {
                condition = (NotificationCondition) task.getCondition();
            }

            View view = inflater.inflate(R.layout.widget_text_input, null);
            TextInputEditText editText = view.findViewById(R.id.title_edit);
            editText.setText(condition.getText());

            new MaterialAlertDialogBuilder(requireContext())
                    .setPositiveButton(R.string.enter, (dialog, which) -> {
                        Editable text = editText.getText();
                        if (text != null) condition.setText(String.valueOf(text));
                        task.setCondition(condition);
                        TaskRepository.getInstance().saveTask(task);
                        refreshNotification();
                        dialog.dismiss();
                    })
                    .setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss())
                    .setView(view)
                    .setTitle(R.string.notification_condition_tips)
                    .show();
        });

        return binding.getRoot();
    }

    public void refreshUI() {
        binding.appBox.setVisibility(task.getType() == TaskType.IT_IS_TIME ? View.GONE : View.VISIBLE);
        refreshApps();
        binding.conditionBox.setVisibility(task.getType() == TaskType.IT_IS_TIME || task.getType() == TaskType.NEW_NOTIFICATION ? View.VISIBLE : View.GONE);
        binding.textButton.setVisibility(task.getType() == TaskType.NEW_NOTIFICATION ? View.VISIBLE : View.GONE);
        refreshNotification();
        binding.dateButton.setVisibility(task.getType() == TaskType.IT_IS_TIME ? View.VISIBLE : View.GONE);
        binding.timeButton.setVisibility(task.getType() == TaskType.IT_IS_TIME ? View.VISIBLE : View.GONE);
        binding.periodicButton.setVisibility(task.getType() == TaskType.IT_IS_TIME ? View.VISIBLE : View.GONE);
        refreshTime();
    }

    public void refreshApps() {
        binding.appIconBox.removeAllViews();
        Map<String, Drawable> drawables = new LinkedHashMap<>();
        viewModel.loadAppsIcon(drawables, task.getPkgNames(), result -> {
            if (result) {
                binding.appIconBox.post(() -> {
                    for (Map.Entry<String, Drawable> entry : drawables.entrySet()) {
                        ShapeableImageView imageView = (ShapeableImageView) LayoutInflater.from(requireContext()).inflate(R.layout.view_task_info_app, binding.appBox, true);
                        imageView.setImageDrawable(entry.getValue());
                    }
                });
            }
        });
    }

    public void refreshTime() {
        if (task.getCondition() != null && task.getCondition() instanceof TimeCondition) {
            TimeCondition condition = (TimeCondition) task.getCondition();
            Context context = requireContext();
            String startTime = context.getString(R.string.date, AppUtils.formatDateLocalDate(context, condition.getStartTime()), AppUtils.formatDateLocalTime(context, condition.getStartTime()));
            startTime = context.getString(R.string.time_condition_start_time, startTime);
            if (condition.getPeriodic() > 0) {
                startTime += context.getString(R.string.time_condition_periodic, AppUtils.formatDateLocalDuration(context, ((long) condition.getPeriodic()) * 60 * 1000));
            }
            binding.dateText.setText(startTime);
        }
    }

    public void refreshNotification() {
        if (task.getCondition() != null && task.getCondition() instanceof NotificationCondition) {
            NotificationCondition condition = (NotificationCondition) task.getCondition();
            String string = requireContext().getString(R.string.notification_condition_des, condition.getText());
            binding.dateText.setText(string);
        }
    }

    @Override
    public void onChanged(Task task) {
        if (adapter != null && task.getId().equals(this.task.getId())) {
            adapter.taskChanged(task);
        }
    }

    @Override
    public void onRemoved(Task task) {
        if (task.getId().equals(this.task.getId())) {
            requireActivity().onBackPressed();
        }
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        TaskRepository.getInstance().addCallback(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        TaskRepository.getInstance().removeCallback(this);
    }
}
