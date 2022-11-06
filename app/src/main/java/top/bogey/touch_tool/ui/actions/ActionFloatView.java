package top.bogey.touch_tool.ui.actions;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.text.Editable;
import android.text.InputType;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.FrameLayout;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.annotation.NonNull;

import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.BehaviorMode;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.ActionType;
import top.bogey.touch_tool.database.bean.action.ColorAction;
import top.bogey.touch_tool.database.bean.action.DelayAction;
import top.bogey.touch_tool.database.bean.action.ImageAction;
import top.bogey.touch_tool.database.bean.action.NullAction;
import top.bogey.touch_tool.database.bean.action.NumberAction;
import top.bogey.touch_tool.database.bean.action.SystemAction;
import top.bogey.touch_tool.database.bean.action.TaskAction;
import top.bogey.touch_tool.database.bean.action.TextAction;
import top.bogey.touch_tool.database.bean.action.TouchAction;
import top.bogey.touch_tool.databinding.FloatActionBinding;
import top.bogey.touch_tool.ui.picker.ImagePickerFloatView;
import top.bogey.touch_tool.ui.picker.WordPickerFloatView;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.FloatBaseCallback;
import top.bogey.touch_tool.utils.ResultCallback;
import top.bogey.touch_tool.utils.TextChangedListener;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;

@SuppressLint("ViewConstructor")
public class ActionFloatView extends FrameLayout implements FloatViewInterface {
    private final FloatActionBinding binding;
    private final ActionsRecyclerViewAdapter adapter;
    private final ArrayAdapter<TaskAction> conditionAdapter;

    private BehaviorMode behaviorMode;
    private Action condition = new NullAction();

    public ActionFloatView(@NonNull Context context, @NonNull Task task, @NonNull Behavior behavior, ResultCallback callback) {
        super(context);

        binding = FloatActionBinding.inflate(LayoutInflater.from(context), this, true);

        adapter = new ActionsRecyclerViewAdapter(task, behavior.getActions());
        binding.delayButton.setOnClickListener(v -> adapter.addNode(new DelayAction()));
        binding.textButton.setOnClickListener(v -> adapter.addNode(new TextAction()));
        binding.imageButton.setOnClickListener(v -> adapter.addNode(new ImageAction()));
        binding.touchButton.setOnClickListener(v -> adapter.addNode(new TouchAction()));
        binding.colorButton.setOnClickListener(v -> adapter.addNode(new ColorAction()));
        binding.keyButton.setOnClickListener(v -> adapter.addNode(new SystemAction()));
        binding.taskButton.setOnClickListener(v -> adapter.addNode(new TaskAction()));
        binding.recyclerView.setAdapter(adapter);

        conditionAdapter = new ArrayAdapter<>(context, R.layout.float_action_spinner_item);

        binding.modeGroup.addOnButtonCheckedListener((group, checkedId, isChecked) -> {
            if (isChecked) {
                int index = group.indexOfChild(group.findViewById(checkedId));
                behaviorMode = BehaviorMode.values()[index];
                switch (behaviorMode) {
                    case CONDITION:
                        binding.conditionTitle.setText(context.getText(R.string.condition_tips_for_condition));
                        binding.timesRow.setVisibility(GONE);
                        if (condition.getType() == ActionType.NUMBER) condition = new NullAction();
                        refreshCondition(condition);
                        break;
                    case LOOP:
                        binding.conditionTitle.setText(context.getText(R.string.condition_tips_for_loop));
                        binding.timesRow.setVisibility(VISIBLE);
                        refreshCondition(condition);
                        break;
                    case PARALLEL:
                        binding.conditionTitle.setText(context.getText(R.string.condition_tips_for_loop));
                        binding.timesRow.setVisibility(GONE);
                        if (condition.getType() != ActionType.NUMBER) condition = new NumberAction();
                        refreshCondition(condition);
                        break;
                }
            }
        });
        int[] ids = {R.id.condition_button, R.id.loop_button, R.id.parallel_button};
        binding.modeGroup.check(ids[behavior.getActionMode().ordinal()]);

        binding.conditionSpinner.spinner.setAdapter(conditionAdapter);
        binding.conditionSpinner.spinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> adapterView, View view, int i, long l) {
                TaskAction taskInfo = conditionAdapter.getItem(i);
                ActionType actionType = ActionType.valueOf(taskInfo.getId());
                if (condition.getType() != actionType) {
                    switch (actionType) {
                        case NULL:
                            refreshCondition(new NullAction());
                            break;
                        case NUMBER:
                            refreshCondition(new NumberAction());
                            break;
                        case TEXT:
                            refreshCondition(new TextAction());
                            break;
                        case IMAGE:
                            refreshCondition(new ImageAction());
                            break;
                    }
                }
            }

            @Override
            public void onNothingSelected(AdapterView<?> adapterView) {

            }
        });
        refreshCondition(behavior.getCondition());

        binding.timesInclude.textInputLayout.setHint(context.getString(R.string.loop_times));
        binding.timesInclude.titleEdit.setInputType(InputType.TYPE_CLASS_NUMBER);
        binding.timesInclude.titleEdit.setText(String.valueOf(behavior.getTimes()));

        binding.textInclude.textBaseInclude.titleEdit.addTextChangedListener(new TextChangedListener() {
            @Override
            public void afterTextChanged(Editable s) {
                if (condition.getType() == ActionType.TEXT) ((TextAction) condition).setText(String.valueOf(s));
            }
        });

        binding.textInclude.pickerButton.setOnClickListener(v -> {
            TextAction textAction = (TextAction) condition;
            new WordPickerFloatView(getContext(), picker -> {
                WordPickerFloatView wordPicker = (WordPickerFloatView) picker;
                String word = wordPicker.getWord();
                textAction.setText(word);
                binding.textInclude.textBaseInclude.titleEdit.setText(word);
            }, textAction).show();
        });

        binding.imageInclude.similarText.addTextChangedListener(new TextChangedListener() {
            @Override
            public void afterTextChanged(Editable s) {
                ImageAction imageAction = (ImageAction) condition;
                if (s != null && s.length() > 0) {
                    imageAction.setValue(Integer.parseInt(String.valueOf(s)));
                } else {
                    imageAction.setValue(95);
                }
            }
        });

        binding.imageInclude.pickerButton.setOnClickListener(v -> {
            ImageAction imageAction = (ImageAction) condition;
            new ImagePickerFloatView(getContext(), picker -> {
                ImagePickerFloatView imagePicker = (ImagePickerFloatView) picker;
                Bitmap bitmap = imagePicker.getBitmap();
                imageAction.setBitmap(bitmap, DisplayUtils.getScreen(getContext()));
                binding.imageInclude.image.setImageBitmap(bitmap);
            }, imageAction).show();
        });

        binding.cancelButton.setOnClickListener(v -> dismiss());
        binding.saveButton.setOnClickListener(v -> {
            List<Action> actions = adapter.getActions();
            if (actions.isEmpty()) {
                Toast.makeText(getContext(), R.string.check_save_targets, Toast.LENGTH_LONG).show();
                return;
            }

            int times = 1;
            Editable timesEdit = binding.timesInclude.titleEdit.getText();
            if (timesEdit != null && timesEdit.length() > 0) times = Integer.parseInt(String.valueOf(timesEdit));

            behavior.setActionMode(behaviorMode);
            behavior.setActions(actions);
            behavior.setCondition(condition);
            behavior.setTimes(times);

            if (callback != null) callback.onResult(true);
            dismiss();
        });
    }

    @Override
    public void show() {
        EasyFloat.with(getContext())
                .setLayout(this)
                .setTag(ActionFloatView.class.getCanonicalName())
                .setDragEnable(true)
                .setGravity(FloatGravity.CENTER, 0, 0)
                .setCallback(new FloatBaseCallback())
                .hasEditText(true)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(ActionFloatView.class.getCanonicalName());
    }

    private void refreshCondition(Action newCondition) {
        if (newCondition != null) condition = newCondition;
        conditionAdapter.clear();
        ActionType[] actionTypes;
        String[] keys = getContext().getResources().getStringArray(R.array.condition_type);
        switch (behaviorMode) {
            case CONDITION:
                actionTypes = new ActionType[]{ActionType.NULL, ActionType.TEXT, ActionType.IMAGE};
                for (ActionType actionType : actionTypes) {
                    conditionAdapter.add(new TaskAction(actionType.name(), keys[actionType.ordinal()]));
                }
                if (adapter != null) {
                    if (condition.getType() == ActionType.NULL) adapter.setMaxCount(1);
                    else adapter.setMaxCount(2);
                }
                break;
            case LOOP:
                actionTypes = new ActionType[]{ActionType.NULL, ActionType.NUMBER, ActionType.TEXT, ActionType.IMAGE};
                for (ActionType actionType : actionTypes) {
                    conditionAdapter.add(new TaskAction(actionType.name(), keys[actionType.ordinal()]));
                }
                if (adapter != null) adapter.setMaxCount(10);
                break;
            case PARALLEL:
                actionTypes = new ActionType[]{ActionType.NUMBER};
                for (ActionType actionType : actionTypes) {
                    conditionAdapter.add(new TaskAction(actionType.name(), keys[actionType.ordinal()]));
                }
                if (adapter != null) adapter.setMaxCount(5);
                break;
        }

        switch (condition.getType()) {
            case NULL:
                binding.imageInclude.getRoot().setVisibility(INVISIBLE);
                binding.textInclude.getRoot().setVisibility(VISIBLE);
                binding.textInclude.pickerButton.setVisibility(GONE);
                binding.textInclude.textBaseInclude.textInputLayout.setHint(getContext().getText(behaviorMode == BehaviorMode.CONDITION ? R.string.condition_null_for_condition : R.string.condition_null_for_loop));
                binding.textInclude.textBaseInclude.textInputLayout.setEnabled(false);
                binding.textInclude.textBaseInclude.titleEdit.setText("");
                selectSpinner(ActionType.NULL.name());
                break;
            case NUMBER:
                binding.imageInclude.getRoot().setVisibility(INVISIBLE);
                binding.textInclude.getRoot().setVisibility(VISIBLE);
                binding.textInclude.pickerButton.setVisibility(GONE);
                binding.textInclude.textBaseInclude.textInputLayout.setHint(getContext().getText(behaviorMode == BehaviorMode.LOOP ? R.string.condition_number_for_loop : R.string.condition_number_for_parallel));
                binding.textInclude.textBaseInclude.textInputLayout.setEnabled(true);
                binding.textInclude.textBaseInclude.titleEdit.setText(String.valueOf(((NumberAction) condition).getTargetNum()));
                binding.textInclude.textBaseInclude.titleEdit.setInputType(InputType.TYPE_CLASS_NUMBER);
                selectSpinner(ActionType.NUMBER.name());
                break;
            case TEXT:
                binding.imageInclude.getRoot().setVisibility(INVISIBLE);
                binding.textInclude.getRoot().setVisibility(VISIBLE);
                binding.textInclude.pickerButton.setVisibility(VISIBLE);
                binding.textInclude.textBaseInclude.textInputLayout.setHint(getContext().getText(R.string.condition_text));
                binding.textInclude.textBaseInclude.textInputLayout.setEnabled(true);
                binding.textInclude.textBaseInclude.titleEdit.setText(((TextAction) condition).getText());
                binding.textInclude.textBaseInclude.titleEdit.setInputType(InputType.TYPE_CLASS_TEXT);
                selectSpinner(ActionType.TEXT.name());
                break;
            case IMAGE:
                binding.textInclude.getRoot().setVisibility(INVISIBLE);
                binding.imageInclude.getRoot().setVisibility(VISIBLE);
                ImageAction imageAction = (ImageAction) condition;
                if (imageAction.getBitmap() != null) {
                    binding.imageInclude.image.setImageBitmap(imageAction.getBitmap());
                    binding.imageInclude.similarText.setText(String.valueOf(imageAction.getValue()));
                }
                selectSpinner(ActionType.IMAGE.name());
                break;
        }
    }

    private void selectSpinner(String id) {
        Spinner spinner = binding.conditionSpinner.spinner;
        for (int i = 0; i < conditionAdapter.getCount(); i++) {
            TaskAction item = conditionAdapter.getItem(i);
            if (item.getId().equals(id)) {
                spinner.setSelection(i);
                return;
            }
        }
        if (conditionAdapter.getCount() > 0) {
            spinner.setSelection(0);
            AdapterView.OnItemSelectedListener listener = spinner.getOnItemSelectedListener();
            if (listener != null) listener.onItemSelected(spinner, spinner.getSelectedView(), 0, conditionAdapter.getItemId(0));
        }
    }
}
