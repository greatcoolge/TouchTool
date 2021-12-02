package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentPickerTaskBinding;
import top.bogey.auto_touch.room.bean.SimpleTaskInfo;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.room.data.TaskRepository;

public class TaskPicker extends NodePicker{
    private SimpleTaskInfo taskInfo;

    public TaskPicker(@NonNull Context context, PickerCallback pickerCallback, String pkgName) {
        super(context, null, pickerCallback);
        FloatFragmentPickerTaskBinding binding = FloatFragmentPickerTaskBinding.inflate(LayoutInflater.from(context));
        layout = binding.getRoot();
        binding.bgLayout.closeButton.setOnClickListener(v -> {
            if (pickerCallback != null){
                pickerCallback.call(this);
            }
            dismiss();
        });

        ArrayAdapter<SimpleTaskInfo> arrayAdapter = new ArrayAdapter<>(context, R.layout.dialog_fragment_action_edit_picker);
        TaskRepository repository = new TaskRepository(context);
        List<Task> tasks = repository.getTasksByPackageName(pkgName);
        for (Task task : tasks) {
            if (task.taskStatus != TaskStatus.AUTO){
                arrayAdapter.add(new SimpleTaskInfo(task.id, task.title));
            }
        }
        binding.targetSpinner.setAdapter(arrayAdapter);
        binding.targetSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                taskInfo = arrayAdapter.getItem(position);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });
    }

    public SimpleTaskInfo getTaskInfo() {
        return taskInfo;
    }
}
