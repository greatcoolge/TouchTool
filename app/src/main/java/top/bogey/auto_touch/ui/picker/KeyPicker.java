package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentPickerTaskBinding;
import top.bogey.auto_touch.room.bean.SimpleTaskInfo;

public class KeyPicker extends NodePicker{
    private SimpleTaskInfo taskInfo;

    public KeyPicker(@NonNull Context context, PickerCallback pickerCallback) {
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
        String[] strings = context.getResources().getStringArray(R.array.keys);
        for (int i = 0; i < strings.length; i++) {
            arrayAdapter.add(new SimpleTaskInfo(i + 1, strings[i]));
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
