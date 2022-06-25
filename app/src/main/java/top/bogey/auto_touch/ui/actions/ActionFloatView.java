package top.bogey.auto_touch.ui.actions;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Point;
import android.text.Editable;
import android.text.InputType;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;

import com.google.android.material.button.MaterialButton;

import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatActionBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.ActionMode;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.node.ImageNode;
import top.bogey.auto_touch.room.bean.node.Node;
import top.bogey.auto_touch.room.bean.node.NodeType;
import top.bogey.auto_touch.room.bean.node.NullNode;
import top.bogey.auto_touch.room.bean.node.NumberNode;
import top.bogey.auto_touch.room.bean.node.TextNode;
import top.bogey.auto_touch.room.bean.node.TouchNode;
import top.bogey.auto_touch.ui.picker.ImagePickerFloatView;
import top.bogey.auto_touch.ui.picker.TouchPickerFloatView;
import top.bogey.auto_touch.ui.picker.WordPickerFloatView;
import top.bogey.auto_touch.utils.FloatBaseCallback;
import top.bogey.auto_touch.utils.ResultCallback;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;
import top.bogey.auto_touch.utils.easy_float.FloatGravity;
import top.bogey.auto_touch.utils.easy_float.FloatViewInterface;

@SuppressLint("ViewConstructor")
public class ActionFloatView extends FrameLayout implements FloatViewInterface {
    private final FloatActionBinding binding;
    private final ActionsRecyclerViewAdapter adapter;

    private ActionMode mode;
    private Node condition = new NullNode();

    @SuppressLint({"PrivateResource", "NonConstantResourceId"})
    public ActionFloatView(@NonNull Context context, @NonNull Task task, @NonNull Action action, ResultCallback callback) {
        super(context);

        binding = FloatActionBinding.inflate(LayoutInflater.from(context), this, true);

        binding.modeGroup.addOnButtonCheckedListener((group, checkedId, isChecked) -> {
            for (int i = 0; i < group.getChildCount(); i++) {
                MaterialButton button = (MaterialButton) group.getChildAt(i);
                button.setStrokeWidth((int) getContext().getResources().getDimension(com.google.android.material.R.dimen.m3_btn_stroke_size));
            }

            if (isChecked){
                MaterialButton checkedButton = group.findViewById(checkedId);
                checkedButton.setStrokeWidth(0);
                switch (checkedId){
                    case R.id.condition_button:
                        binding.conditionTitle.setText(context.getText(R.string.title_condition));
                        binding.timesRow.setVisibility(GONE);
                        mode = ActionMode.CONDITION;
                        if (condition.getType() == NodeType.NUMBER) condition = new NullNode();
                        refreshCondition(condition);
                        break;
                    case R.id.loop_button:
                        binding.conditionTitle.setText(context.getText(R.string.title_stop));
                        binding.timesRow.setVisibility(VISIBLE);
                        mode = ActionMode.LOOP;
                        refreshCondition(condition);
                        break;
                    case R.id.parallel_button:
                        binding.conditionTitle.setText(context.getText(R.string.title_stop));
                        binding.timesRow.setVisibility(GONE);
                        mode = ActionMode.PARALLEL;
                        if (condition.getType() != NodeType.NUMBER) condition = new NumberNode(1);
                        refreshCondition(condition);
                        break;
                }
            }
        });
        int[] ids = {R.id.condition_button, R.id.loop_button, R.id.parallel_button};
        binding.modeGroup.check(ids[action.getActionMode().ordinal()]);

        adapter = new ActionsRecyclerViewAdapter(task, action.getTargets());
        binding.delayButton.setOnClickListener(v -> adapter.addNode(NodeType.DELAY));
        binding.textButton.setOnClickListener(v -> adapter.addNode(NodeType.TEXT));
        binding.imageButton.setOnClickListener(v -> adapter.addNode(NodeType.IMAGE));
        binding.touchButton.setOnClickListener(v -> adapter.addNode(NodeType.TOUCH));
        binding.colorButton.setOnClickListener(v -> adapter.addNode(NodeType.COLOR));
        binding.keyButton.setOnClickListener(v -> adapter.addNode(NodeType.KEY));
        binding.taskButton.setOnClickListener(v -> adapter.addNode(NodeType.TASK));

        binding.recyclerView.setAdapter(adapter);

        binding.timesInclude.textInputLayout.setHint(context.getString(R.string.title_times));
        binding.timesInclude.titleEdit.setInputType(InputType.TYPE_CLASS_NUMBER);
        binding.timesInclude.titleEdit.setText(String.valueOf(action.getTimes()));

        binding.conditionButtonNone.setOnClickListener(v -> refreshCondition(new NullNode()));
        binding.conditionButtonTimes.setOnClickListener(v -> refreshCondition(new NumberNode(1)));
        binding.conditionButtonText.setOnClickListener(v -> refreshCondition(new TextNode("")));
        binding.conditionButtonImage.setOnClickListener(v -> refreshCondition(new ImageNode(null)));
        refreshCondition(action.getCondition());

        binding.textInclude.textBaseInclude.titleEdit.addTextChangedListener(new ActionsRecyclerViewAdapter.TextChangedWatcher(){
            @Override
            public void afterTextChanged(Editable s) {
                if (condition.getType() == NodeType.TEXT) condition.setValue(String.valueOf(s));
            }
        });

        binding.textInclude.pickerButton.setOnClickListener(v -> {
            if (condition.getType() == NodeType.TEXT){
                TextNode textNode = (TextNode) condition;
                new WordPickerFloatView(getContext(), picker -> {
                    WordPickerFloatView wordPicker = (WordPickerFloatView) picker;
                    String word = wordPicker.getWord();
                    textNode.setValue(word);
                    binding.textInclude.textBaseInclude.titleEdit.setText(word);
                }, textNode).show();
            } else if (condition.getType() == NodeType.TOUCH){
                TouchNode touchNode = (TouchNode) condition;
                new TouchPickerFloatView(getContext(), picker -> {
                    TouchPickerFloatView touchPicker = (TouchPickerFloatView) picker;
                    List<Point> points = touchPicker.getPoints();
                    touchNode.setValue(getContext(), points);
                    binding.textInclude.textBaseInclude.titleEdit.setText(touchNode.getTitle());
                }, touchNode.getPoints(getContext())).show();
            }
        });

        binding.imageInclude.similarText.addTextChangedListener(new ActionsRecyclerViewAdapter.TextChangedWatcher(){
            @Override
            public void afterTextChanged(Editable s) {
                ImageNode imageNode = (ImageNode) condition;
                if (s != null && s.length() > 0){
                    imageNode.getValue().setValue(Integer.parseInt(String.valueOf(s)));
                } else {
                    imageNode.getValue().setValue(100);
                }
            }
        });

        binding.imageInclude.pickerButton.setOnClickListener(v -> {
            ImageNode imageNode = (ImageNode) condition;
            new ImagePickerFloatView(getContext(), picker -> {
                ImagePickerFloatView imagePicker = (ImagePickerFloatView) picker;
                Bitmap bitmap = imagePicker.getBitmap();
                imageNode.getValue().setBitmap(bitmap);
                binding.imageInclude.image.setImageBitmap(bitmap);
            }, imageNode).show();
        });

        binding.cancelButton.setOnClickListener(v -> dismiss());
        binding.saveButton.setOnClickListener(v -> {
            List<Node> nodes = adapter.getNodes();
            if (nodes.isEmpty()) {
                Toast.makeText(getContext(), R.string.check_save_targets, Toast.LENGTH_LONG).show();
                return;
            }

            int times = 1;
            Editable timesEdit = binding.timesInclude.titleEdit.getText();
            if (timesEdit != null && timesEdit.length() > 0) times = Integer.parseInt(String.valueOf(timesEdit));

            action.setActionMode(mode);
            action.setTargets(nodes);
            action.setCondition(condition);
            action.setTimes(times);

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

    private void refreshCondition(Node newCondition){
        if (newCondition != null) condition = newCondition;
        switch (mode) {
            case CONDITION:
                binding.conditionButtonNone.setVisibility(VISIBLE);
                binding.conditionButtonTimes.setVisibility(GONE);
                binding.conditionButtonText.setVisibility(VISIBLE);
                binding.conditionButtonImage.setVisibility(VISIBLE);
                if (adapter != null){
                    if (condition.getType() == NodeType.NULL) adapter.setMaxCount(1);
                    else adapter.setMaxCount(2);
                }
                break;
            case LOOP:
                binding.conditionButtonNone.setVisibility(VISIBLE);
                binding.conditionButtonTimes.setVisibility(VISIBLE);
                binding.conditionButtonText.setVisibility(VISIBLE);
                binding.conditionButtonImage.setVisibility(VISIBLE);
                if (adapter != null) adapter.setMaxCount(10);
                break;
            case PARALLEL:
                binding.conditionButtonNone.setVisibility(GONE);
                binding.conditionButtonTimes.setVisibility(VISIBLE);
                binding.conditionButtonText.setVisibility(GONE);
                binding.conditionButtonImage.setVisibility(GONE);
                if (adapter != null) adapter.setMaxCount(5);
                break;
        }

        switch (condition.getType()){
            case NULL:
                binding.imageInclude.getRoot().setVisibility(INVISIBLE);
                binding.textInclude.getRoot().setVisibility(VISIBLE);
                binding.textInclude.pickerButton.setVisibility(GONE);
                binding.textInclude.textBaseInclude.textInputLayout.setHint(getContext().getText(mode == ActionMode.CONDITION ? R.string.condition_null_condition : R.string.condition_null_loop));
                binding.textInclude.textBaseInclude.textInputLayout.setEnabled(false);
                binding.textInclude.textBaseInclude.titleEdit.setText("");
                break;
            case NUMBER:
                binding.imageInclude.getRoot().setVisibility(INVISIBLE);
                binding.textInclude.getRoot().setVisibility(VISIBLE);
                binding.textInclude.pickerButton.setVisibility(GONE);
                binding.textInclude.textBaseInclude.textInputLayout.setHint(getContext().getText(mode == ActionMode.LOOP ? R.string.condition_number_loop : R.string.condition_number_parallel));
                binding.textInclude.textBaseInclude.textInputLayout.setEnabled(true);
                binding.textInclude.textBaseInclude.titleEdit.setText(String.valueOf(((NumberNode) condition).getValue()));
                binding.textInclude.textBaseInclude.titleEdit.setInputType(InputType.TYPE_CLASS_NUMBER);
                break;
            case TEXT:
                binding.imageInclude.getRoot().setVisibility(INVISIBLE);
                binding.textInclude.getRoot().setVisibility(VISIBLE);
                binding.textInclude.pickerButton.setVisibility(VISIBLE);
                binding.textInclude.textBaseInclude.textInputLayout.setHint(getContext().getText(R.string.condition_text));
                binding.textInclude.textBaseInclude.textInputLayout.setEnabled(true);
                binding.textInclude.textBaseInclude.titleEdit.setText(((TextNode) condition).getValue());
                binding.textInclude.textBaseInclude.titleEdit.setInputType(InputType.TYPE_CLASS_TEXT);
                break;
            case IMAGE:
                binding.textInclude.getRoot().setVisibility(INVISIBLE);
                binding.imageInclude.getRoot().setVisibility(VISIBLE);
                if (condition.getValue() != null){
                    binding.imageInclude.image.setImageBitmap(((ImageNode) condition).getValue().getBitmap());
                    binding.imageInclude.similarText.setText(String.valueOf(((ImageNode) condition).getValue().getValue()));
                }
                break;
        }
    }
}
