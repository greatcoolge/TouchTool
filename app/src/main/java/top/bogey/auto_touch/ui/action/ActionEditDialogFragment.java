package top.bogey.auto_touch.ui.action;

import android.app.AlertDialog;
import android.app.Dialog;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;
import androidx.lifecycle.ViewModelProvider;

import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.DialogFragmentActionEditBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.ActionMode;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.room.bean.SimpleTaskInfo;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.picker.ImagePicker;
import top.bogey.auto_touch.util.CompleteCallback;

public class ActionEditDialogFragment extends DialogFragment {
    private DialogFragmentActionEditBinding binding;
    private MainViewModel viewModel;
    private final Task task;
    private final Action action;
    private final CompleteCallback callback;

    private ArrayAdapter<SimpleTaskInfo> arrayAdapter;

    private int delayScale = 1;
    private int intervalScale = 1;
    private int timeScale = 1;

    private final Node target;
    private SimpleTaskInfo taskInfo;
    private final Node stop;
    private ActionMode mode;

    public ActionEditDialogFragment(@NonNull Task task, @NonNull Action action, CompleteCallback callback){
        this.task = task;
        this.action = action;
        this.callback = callback;
        if (action.target != null) target = action.target.clone();
        else target = new Node(NodeType.WORD);
        if (action.stop != null) stop = action.stop.clone();
        else stop = new Node(NodeType.NULL);
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        binding = DialogFragmentActionEditBinding.inflate(getLayoutInflater());
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);

        KeysRecyclerViewAdapter adapter = new KeysRecyclerViewAdapter(this, action.keys);
        binding.keysBox.setAdapter(adapter);

        arrayAdapter = new ArrayAdapter<>(requireContext(), R.layout.dialog_fragment_action_edit_picker);
        binding.targetSpinner.setAdapter(arrayAdapter);
        binding.targetSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                taskInfo = arrayAdapter.getItem(position);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) { }
        });


        binding.delayGroup.setOnCheckedChangeListener((group, checkedId) -> {
            int index = group.indexOfChild(group.findViewById(checkedId));
            delayScale = index == 0 ? 1 : 1000;
        });
        binding.delayGroup.check(binding.delayGroup.getChildAt(action.delay % 1000 == 0 ? 1 : 0).getId());

        binding.timeGroup.setOnCheckedChangeListener((group, checkedId) -> {
            int index = group.indexOfChild(group.findViewById(checkedId));
            timeScale = index == 0 ? 1 : 1000;
        });
        binding.timeGroup.check(binding.timeGroup.getChildAt(action.time % 1000 == 0 ? 1 : 0).getId());

        binding.intervalGroup.setOnCheckedChangeListener((group, checkedId) -> {
            int index = group.indexOfChild(group.findViewById(checkedId));
            intervalScale = index == 0 ? 1 : 1000;
        });
        binding.intervalGroup.check(binding.intervalGroup.getChildAt(action.interval % 1000 == 0 ? 1 : 0).getId());

        binding.targetGroup.setOnCheckedChangeListener((group, checkedId) -> {
            int index = group.indexOfChild(group.findViewById(checkedId));
            changeTargetType(index);
        });
        if (action.actionMode == ActionMode.TOUCH){
            binding.targetGroup.check(binding.targetGroup.getChildAt(target.type.ordinal() - 2).getId());
        }

        binding.stopGroup.setOnCheckedChangeListener((group, checkedId) -> {
            int index = group.indexOfChild(group.findViewById(checkedId));
            switch (index) {
                case 0:
                    binding.stopEdit.setVisibility(View.VISIBLE);
                    binding.stopImageBox.setVisibility(View.INVISIBLE);
                    binding.stopEdit.setText(getString(R.string.none));
                    binding.stopEdit.setEnabled(false);
                    stop.setNull();
                    break;
                case 1:
                    binding.stopEdit.setVisibility(View.VISIBLE);
                    binding.stopImageBox.setVisibility(View.INVISIBLE);
                    binding.stopEdit.setText(getString(R.string.success_once));
                    binding.stopEdit.setEnabled(false);
                    stop.setBool(true);
                    break;
                case 2:
                    binding.stopEdit.setVisibility(View.VISIBLE);
                    binding.stopImageBox.setVisibility(View.INVISIBLE);
                    binding.stopEdit.setText("");
                    binding.stopEdit.setEnabled(true);
                    stop.setWord("");
                    break;
                case 3:
                    binding.stopEdit.setVisibility(View.INVISIBLE);
                    binding.stopImageBox.setVisibility(View.VISIBLE);
                    stop.setImage(null);
                    break;
            }
        });
        binding.stopGroup.check(binding.stopGroup.getChildAt(stop.type.ordinal()).getId());

        binding.modeGroup.setOnCheckedChangeListener((group, checkedId) -> {
            int index = group.indexOfChild(group.findViewById(checkedId));
            changeMode(ActionMode.values()[index + 1]);
        });
        binding.modeGroup.check(binding.modeGroup.getChildAt(action.actionMode.ordinal() - 1).getId());

        binding.delayEdit.setText(String.valueOf(action.delay / delayScale));
        binding.timeEdit.setText(String.valueOf(action.time / timeScale));
        binding.intervalEdit.setText(String.valueOf(action.interval / intervalScale));
        binding.timesEdit.setText(String.valueOf(action.times));

        binding.targetImagePicker.setOnClickListener(v -> new ImagePicker(requireContext(), nodePicker -> {
            ImagePicker imagePicker = (ImagePicker) nodePicker;
            Bitmap bitmap = imagePicker.getBitmap();
            binding.targetImage.setImageBitmap(bitmap);
            imagePicker.dismiss();
        }).show(Gravity.START | Gravity.TOP, 0, 0));

        binding.targetPosPicker.setOnClickListener(v -> {

        });

        binding.stopImagePicker.setOnClickListener(v -> {

        });

        binding.closedButton.setOnClickListener(v -> dismiss());

        binding.saveButton.setOnClickListener(v -> {
            String delayString = String.valueOf(binding.delayEdit.getText());
            String timeString = String.valueOf(binding.timeEdit.getText());
            String intervalString = String.valueOf(binding.intervalEdit.getText());
            String timesString = String.valueOf(binding.timesEdit.getText());
            if (delayString.isEmpty() || timeString.isEmpty() || intervalString.isEmpty() || timesString.isEmpty()){
                Toast.makeText(getContext(), "存在空配置项", Toast.LENGTH_LONG).show();
                return;
            }

            int delay = Integer.parseInt(delayString) * delayScale;
            int time = Integer.parseInt(timeString) * timeScale;
            int interval = Integer.parseInt(intervalString) * intervalScale;
            int times = Integer.parseInt(timesString);
            if (!action.checkTimeSafe(delay, time, interval, times)){
                Toast.makeText(getContext(), "动作时长超过1分钟", Toast.LENGTH_LONG).show();
                return;
            }

            boolean flag = false;
            String targetWord = String.valueOf(binding.targetEdit.getText());
            switch (target.type){
                case WORD:
                    if (targetWord.isEmpty()) flag = true;
                    target.setWord(targetWord);
                    break;
                case IMAGE:
                    if (binding.targetImage.getDrawable() == null){
                        flag = true;
                        break;
                    }
                    Bitmap bitmap = ((BitmapDrawable) binding.targetImage.getDrawable()).getBitmap();
                    target.setImage(bitmap);
                    break;
                case POS:
                    if (targetWord.isEmpty()) flag = true;
                    target.setPoses(targetWord);
                    break;
                case TASK:
                    if (taskInfo == null){
                        flag = true;
                        break;
                    }
                    target.setTask(taskInfo);
                    break;
            }
            if (flag){
                Toast.makeText(getContext(), "目标为空", Toast.LENGTH_LONG).show();
                return;
            }

            switch (stop.type) {
                case WORD:
                    String stopWord = String.valueOf(binding.stopEdit.getText());
                    if (stopWord.isEmpty()) flag = true;
                    stop.setWord(stopWord);
                    break;
                case IMAGE:
                    if (binding.stopImage.getDrawable() == null){
                        flag = true;
                        break;
                    }
                    Bitmap bitmap = ((BitmapDrawable) binding.stopImage.getDrawable()).getBitmap();
                    target.setImage(bitmap);
                    break;
            }
            if (flag){
                Toast.makeText(getContext(), "停止条件为空", Toast.LENGTH_LONG).show();
                return;
            }

            action.actionMode = mode;
            action.keys = adapter.getNodes();
            action.target = target;
            action.stop = stop;
            action.delay = delay;
            action.time = time;
            action.interval = interval;
            action.times = times;
            if (callback != null) callback.onComplete();
            dismiss();
        });

        return new AlertDialog.Builder(getContext()).setView(binding.getRoot()).create();
    }

    private void changeMode(ActionMode mode){
        this.mode = mode;
        switch (mode) {
            case TOUCH:
                binding.targetGroup.setVisibility(View.VISIBLE);
                binding.targetSpinner.setVisibility(View.INVISIBLE);
                if (target.type == NodeType.WORD || target.type == NodeType.IMAGE || target.type == NodeType.POS){
                    binding.targetGroup.check(binding.targetGroup.getChildAt(target.type.ordinal() - 2).getId());
                    changeTargetType(target.type.ordinal() - 2);
                } else {
                    binding.targetGroup.check(binding.targetGroup.getChildAt(0).getId());
                    changeTargetType(0);
                }
                break;
            case KEY:
            case TASK:
                binding.targetGroup.setVisibility(View.INVISIBLE);
                binding.targetEditBox.setVisibility(View.INVISIBLE);
                binding.targetImageBox.setVisibility(View.INVISIBLE);
                binding.targetSpinner.setVisibility(View.VISIBLE);

                arrayAdapter.clear();
                if (mode == ActionMode.KEY){
                    String[] strings = getResources().getStringArray(R.array.keys);
                    for (int i = 0; i < strings.length; i++) {
                        arrayAdapter.add(new SimpleTaskInfo(i + 1, strings[i]));
                    }
                } else {
                    List<Task> tasks = viewModel.getTasksByPackageName(task.pkgName);
                    for (Task task : tasks) {
                        if (task.id != this.task.id && task.taskStatus != TaskStatus.AUTO){
                            arrayAdapter.add(new SimpleTaskInfo(task.id, task.title));
                        }
                    }
                }
                arrayAdapter.notifyDataSetChanged();
                if (target.type == NodeType.TASK && !target.getWord().isEmpty()){
                    taskInfo = target.getTask();
                } else {
                    taskInfo = null;
                }
                target.setTask(null);
                changeSpinnerSelector();
                break;
        }
    }

    private void changeTargetType(int selectIndex){
        switch (selectIndex) {
            case 0:
                binding.targetEditBox.setVisibility(View.VISIBLE);
                binding.targetImageBox.setVisibility(View.INVISIBLE);
                binding.targetEdit.setEnabled(true);
                binding.targetPosPicker.setVisibility(View.GONE);
                if (target.type != NodeType.WORD) target.setWord("");
                binding.targetEdit.setText(target.getWord());
                break;
            case 1:
                binding.targetEditBox.setVisibility(View.INVISIBLE);
                binding.targetImageBox.setVisibility(View.VISIBLE);
                if (target.type != NodeType.IMAGE) target.setImage(null);
                if (target.getImage() != null) binding.targetImage.setImageBitmap(target.getImage());
                break;
            case 2:
                binding.targetEditBox.setVisibility(View.VISIBLE);
                binding.targetImageBox.setVisibility(View.INVISIBLE);
                binding.targetEdit.setEnabled(false);
                binding.targetPosPicker.setVisibility(View.VISIBLE);
                if (target.type != NodeType.POS) target.setPoses((List<Pos>) null);
                binding.targetEdit.setText(target.getWord());
                break;
        }
    }

    private void changeSpinnerSelector(){
        if (taskInfo != null){
            for (int i = 0; i < arrayAdapter.getCount(); i++) {
                SimpleTaskInfo item = arrayAdapter.getItem(i);
                if (item.id == taskInfo.id) {
                    binding.targetSpinner.setSelection(i);
                    return;
                }
            }
        }
        binding.targetSpinner.setSelection(0);
    }
}
