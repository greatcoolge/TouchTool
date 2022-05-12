package top.bogey.auto_touch.ui.action;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.text.Editable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.FrameLayout;
import android.widget.Spinner;
import android.widget.SpinnerAdapter;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;

import java.util.List;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentActionEditBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.ActionMode;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.SimpleTaskInfo;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.easy_float.FloatGravity;
import top.bogey.auto_touch.ui.picker.FloatShowActionEditCallback;
import top.bogey.auto_touch.ui.picker.ImagePicker;
import top.bogey.auto_touch.ui.picker.NodePickerInterface;
import top.bogey.auto_touch.ui.picker.WordPicker;
import top.bogey.auto_touch.util.CompleteCallback;

@SuppressLint("ViewConstructor")
public class FloatActionEdit extends FrameLayout implements NodePickerInterface {
    private FloatFragmentActionEditBinding binding;
    public MainViewModel viewModel;
    public final Task task;
    private final Action action;
    private final CompleteCallback callback;

    private ArrayAdapter<SimpleTaskInfo> conditionTypeArrayAdapter;
    private ArrayAdapter<SimpleTaskInfo> stopTypeArrayAdapter;

    private KeysRecyclerViewAdapter adapter;

    private ActionMode mode = ActionMode.NULL;
    private final Node condition;
    private final Node stop;

    public FloatActionEdit(Context context, @NonNull Task task, @NonNull Action action, CompleteCallback callback){
        super(context);
        this.task = task;
        this.action = action;
        this.callback = callback;
        if (action.getCondition() != null) condition = action.getCondition().clone();
        else condition = new Node(NodeType.NULL);
        if (action.getStop() != null) stop = action.getStop().clone();
        else stop = new Node(NodeType.NULL);
    }

    @Override
    public void show() {
        MainActivity activity = MainApplication.getActivity();
        if (activity != null){
            initView(activity);
            EasyFloat.with(activity)
                    .setLayout(this)
                    .setTag(FloatActionEdit.class.getCanonicalName())
                    .setDragEnable(true)
                    .setGravity(FloatGravity.CENTER, 0, 0)
                    .setCallback(new FloatShowActionEditCallback())
                    .hasEditText(true)
                    .show();
        }
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(FloatActionEdit.class.getCanonicalName());
    }

    public void initView(MainActivity activity) {
        binding = FloatFragmentActionEditBinding.inflate(LayoutInflater.from(getContext()));
        addView(binding.getRoot());
        viewModel = new ViewModelProvider(activity).get(MainViewModel.class);

        adapter = new KeysRecyclerViewAdapter(this, action.getTargets());
        binding.targetBox.setAdapter(adapter);

        conditionTypeArrayAdapter = new ArrayAdapter<>(getContext(), R.layout.float_fragment_action_edit_picker);
        NodeType[] nodeTypes = {NodeType.NULL, NodeType.TEXT, NodeType.IMAGE};
        String[] strings = getResources().getStringArray(R.array.node_type);
        for (NodeType nodeType : nodeTypes) {
            conditionTypeArrayAdapter.add(new SimpleTaskInfo(String.valueOf(nodeType.ordinal()), strings[nodeType.ordinal()]));
        }
        binding.conditionTypeSpinner.setAdapter(conditionTypeArrayAdapter);
        binding.conditionTypeSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if (mode == ActionMode.CONDITION){
                    SimpleTaskInfo item = conditionTypeArrayAdapter.getItem(position);
                    NodeType value = NodeType.values()[Integer.parseInt(item.getId())];
                    switch (value) {
                        case NULL:
                            binding.conditionEdit.setVisibility(VISIBLE);
                            binding.conditionImage.setVisibility(GONE);
                            binding.conditionEdit.setEnabled(false);
                            binding.conditionPicker.setVisibility(GONE);
                            binding.conditionEdit.setText(item.getTitle());
                            condition.setNull();
                            adapter.setMaxCount(1);
                            break;
                        case TEXT:
                            binding.conditionEdit.setVisibility(VISIBLE);
                            binding.conditionImage.setVisibility(GONE);
                            binding.conditionEdit.setEnabled(true);
                            binding.conditionPicker.setVisibility(VISIBLE);
                            binding.conditionPicker.setIconResource(R.drawable.text);
                            if (condition.getType() != NodeType.TEXT) condition.setText("");
                            binding.conditionEdit.setText(condition.getText());
                            adapter.setMaxCount(2);
                            break;
                        case IMAGE:
                            binding.conditionEdit.setVisibility(GONE);
                            binding.conditionImage.setVisibility(VISIBLE);
                            binding.conditionPicker.setVisibility(VISIBLE);
                            binding.conditionPicker.setIconResource(R.drawable.image);
                            if (condition.getType() != NodeType.IMAGE) condition.setImage(null);
                            if (condition.getImage() != null) binding.conditionImage.setImageBitmap(condition.getImage());
                            adapter.setMaxCount(2);
                            break;
                    }
                }
            }
            @Override
            public void onNothingSelected(AdapterView<?> parent) { }
        });
        selectSpinner(binding.conditionTypeSpinner, String.valueOf(condition.getType().ordinal()));

        stopTypeArrayAdapter = new ArrayAdapter<>(getContext(), R.layout.float_fragment_action_edit_picker);
        nodeTypes = new NodeType[]{NodeType.NULL, NodeType.NUMBER, NodeType.TEXT, NodeType.IMAGE};
        for (NodeType nodeType : nodeTypes) {
            stopTypeArrayAdapter.add(new SimpleTaskInfo(String.valueOf(nodeType.ordinal()), strings[nodeType.ordinal()]));
        }
        binding.stopTypeSpinner.setAdapter(stopTypeArrayAdapter);
        binding.stopTypeSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if (mode != ActionMode.CONDITION){
                    SimpleTaskInfo item = stopTypeArrayAdapter.getItem(position);
                    NodeType value = NodeType.values()[Integer.parseInt(item.getId())];
                    switch (value) {
                        case NULL:
                            binding.stopEdit.setVisibility(VISIBLE);
                            binding.stopImage.setVisibility(GONE);
                            binding.stopEdit.setEnabled(false);
                            binding.stopPicker.setVisibility(GONE);
                            binding.stopEdit.setInputType(EditorInfo.TYPE_CLASS_TEXT);
                            binding.stopEdit.setText(item.getTitle());
                            stop.setNull();
                            break;
                        case NUMBER:
                            binding.stopEdit.setVisibility(VISIBLE);
                            binding.stopImage.setVisibility(GONE);
                            binding.stopEdit.setEnabled(true);
                            binding.stopPicker.setVisibility(GONE);
                            binding.stopEdit.setInputType(EditorInfo.TYPE_CLASS_NUMBER);
                            if (stop.getType() != NodeType.NUMBER) stop.setNumber(1);
                            binding.stopEdit.setText(stop.getText());
                            break;
                        case TEXT:
                            binding.stopEdit.setVisibility(VISIBLE);
                            binding.stopImage.setVisibility(GONE);
                            binding.stopEdit.setEnabled(true);
                            binding.stopPicker.setVisibility(VISIBLE);
                            binding.stopPicker.setIconResource(R.drawable.text);
                            binding.stopEdit.setInputType(EditorInfo.TYPE_CLASS_TEXT);
                            if (stop.getType() != NodeType.TEXT) stop.setText("");
                            binding.stopEdit.setText(stop.getText());
                            break;
                        case IMAGE:
                            binding.stopEdit.setVisibility(GONE);
                            binding.stopImage.setVisibility(VISIBLE);
                            binding.stopPicker.setVisibility(VISIBLE);
                            binding.stopPicker.setIconResource(R.drawable.image);
                            if (stop.getType() != NodeType.IMAGE) stop.setImage(null);
                            if (stop.getImage() != null) binding.stopImage.setImageBitmap(stop.getImage());
                            break;
                    }
                }

            }
            @Override
            public void onNothingSelected(AdapterView<?> parent) { }
        });
        selectSpinner(binding.stopTypeSpinner, String.valueOf(stop.getType().ordinal()));

        binding.modeGroup.setOnCheckedChangeListener((group, checkedId) -> {
            int index = group.indexOfChild(group.findViewById(checkedId));
            changeMode(ActionMode.values()[index + 1]);
        });
        binding.modeGroup.check(binding.modeGroup.getChildAt(action.getActionMode().ordinal() - 1).getId());

        binding.timesEdit.setText(String.valueOf(action.getTimes()));

        binding.conditionPicker.setOnClickListener(v -> {
            switch (condition.getType()){
                case TEXT:
                    new WordPicker(getContext(), nodePicker -> {
                        WordPicker wordPicker = (WordPicker) nodePicker;
                        String key = wordPicker.getKey();
                        binding.conditionEdit.setText(key);
                    }).show();
                    break;
                case IMAGE:
                    new ImagePicker(getContext(), nodePicker -> {
                        ImagePicker imagePicker = (ImagePicker) nodePicker;
                        Bitmap bitmap = imagePicker.getBitmap();
                        binding.conditionImage.setImageBitmap(bitmap);
                    }).show();
                    break;
            }
        });

        binding.stopPicker.setOnClickListener(v -> {
            switch (stop.getType()){
                case TEXT:
                    new WordPicker(getContext(), nodePicker -> {
                        WordPicker wordPicker = (WordPicker) nodePicker;
                        String key = wordPicker.getKey();
                        binding.stopEdit.setText(key);
                    }).show();
                    break;
                case IMAGE:
                    new ImagePicker(getContext(), nodePicker -> {
                        ImagePicker imagePicker = (ImagePicker) nodePicker;
                        Bitmap bitmap = imagePicker.getBitmap();
                        binding.stopImage.setImageBitmap(bitmap);
                    }).show();
                    break;
            }
        });

        binding.delayButton.setOnClickListener(v -> adapter.addNewNode(new Node(NodeType.DELAY)));
        binding.wordButton.setOnClickListener(v -> adapter.addNewNode(new Node(NodeType.TEXT)));
        binding.imageButton.setOnClickListener(v -> adapter.addNewNode(new Node(NodeType.IMAGE)));
        binding.posButton.setOnClickListener(v -> adapter.addNewNode(new Node(NodeType.POS)));
        binding.keyButton.setOnClickListener(v -> adapter.addNewNode(new Node(NodeType.KEY)));
        binding.taskButton.setOnClickListener(v -> adapter.addNewNode(new Node(NodeType.TASK)));

        binding.closeButton.setOnClickListener(v -> dismiss());

        binding.saveButton.setOnClickListener(v -> {
            List<Node> nodes = adapter.getNodes();
            if (nodes.isEmpty()){
                Toast.makeText(getContext(), R.string.empty_target, Toast.LENGTH_LONG).show();
                return;
            }

            Editable timesText = binding.timesEdit.getText();
            Editable conditionText = binding.conditionEdit.getText();
            Editable stopText = binding.stopEdit.getText();

            boolean flag = false;
            switch (mode){
                case CONDITION:
                    if (condition.getType() == NodeType.TEXT && (conditionText == null || conditionText.length() == 0)
                            || condition.getType() == NodeType.IMAGE && binding.conditionImage.getDrawable() == null) {
                        flag = true;
                    }
                    break;
                case LOOP:
                    if (timesText == null || timesText.length() == 0
                            || stop.getType() == NodeType.TEXT && (stopText == null || stopText.length() == 0)
                            || stop.getType() == NodeType.IMAGE && binding.stopImage.getDrawable() == null) {
                        flag = true;
                    }
                    break;
                case PARALLEL:
                    if (stop.getType() == NodeType.NUMBER && (stopText == null || stopText.length() == 0))
                        flag = true;
                    break;
            }
            if (flag){
                Toast.makeText(getContext(), R.string.exist_empty_config, Toast.LENGTH_LONG).show();
                return;
            }

            action.setActionMode(mode);
            action.setTargets(nodes);
            switch (mode) {
                case CONDITION:
                    switch (condition.getType()) {
                        case NULL:
                            condition.setNull();
                            break;
                        case TEXT:
                            condition.setText(conditionText.toString());
                            break;
                        case IMAGE:
                            condition.setImage(((BitmapDrawable) binding.conditionImage.getDrawable()).getBitmap());
                            break;
                    }
                    action.setCondition(condition);
                    break;
                case LOOP:
                    action.setTimes(Integer.parseInt(String.valueOf(timesText)));
                case PARALLEL:
                    switch (stop.getType()) {
                        case NULL:
                            stop.setNull();
                            break;
                        case NUMBER:
                            int i = Integer.parseInt(stopText.toString());
                            if (mode == ActionMode.PARALLEL){
                                i = Math.min(i, nodes.size());
                            }
                            stop.setNumber(i);
                            break;
                        case TEXT:
                            stop.setText(stopText.toString());
                            break;
                        case IMAGE:
                            stop.setImage(((BitmapDrawable) binding.stopImage.getDrawable()).getBitmap());
                            break;
                    }
                    action.setStop(stop);
                    break;
            }

            if (callback != null) callback.onComplete();
            dismiss();
        });
    }

    private void changeMode(ActionMode mode){
        if (this.mode == mode) return;
        this.mode = mode;
        String[] strings = getResources().getStringArray(R.array.node_type);
        switch (mode) {
            case CONDITION:
                binding.conditionLayout.setVisibility(VISIBLE);
                binding.timesLayout.setVisibility(GONE);
                binding.stopLayout.setVisibility(GONE);
                selectSpinner(binding.conditionTypeSpinner, String.valueOf(condition.getType().ordinal()));
                break;
            case LOOP:
                binding.conditionLayout.setVisibility(GONE);
                binding.timesLayout.setVisibility(VISIBLE);
                binding.stopLayout.setVisibility(VISIBLE);
                stopTypeArrayAdapter.clear();
                NodeType[] nodeTypes = new NodeType[]{NodeType.NULL, NodeType.NUMBER, NodeType.TEXT, NodeType.IMAGE};
                for (NodeType nodeType : nodeTypes) {
                    stopTypeArrayAdapter.add(new SimpleTaskInfo(String.valueOf(nodeType.ordinal()), strings[nodeType.ordinal()]));
                }
                selectSpinner(binding.stopTypeSpinner, String.valueOf(stop.getType().ordinal()));
                adapter.setMaxCount(10);
                break;
            case PARALLEL:
                binding.conditionLayout.setVisibility(GONE);
                binding.timesLayout.setVisibility(GONE);
                binding.stopLayout.setVisibility(VISIBLE);
                stopTypeArrayAdapter.clear();
                stopTypeArrayAdapter.add(new SimpleTaskInfo(String.valueOf(NodeType.NUMBER.ordinal()), strings[NodeType.NUMBER.ordinal()]));
                selectSpinner(binding.stopTypeSpinner, String.valueOf(stop.getType().ordinal()));
                adapter.setMaxCount(5);
                break;
        }
    }

    public void selectSpinner(@NonNull Spinner spinner, String id){
        SpinnerAdapter spinnerAdapter = spinner.getAdapter();
        for (int i = 0; i < spinnerAdapter.getCount(); i++) {
            SimpleTaskInfo item = (SimpleTaskInfo) spinnerAdapter.getItem(i);
            if (item.getId().equals(id)) {
                spinner.setSelection(i);
                return;
            }
        }
        if (spinnerAdapter.getCount() > 0){
            spinner.setSelection(0);
            AdapterView.OnItemSelectedListener listener = spinner.getOnItemSelectedListener();
            listener.onItemSelected(spinner, spinner.getSelectedView(), 0, spinnerAdapter.getItemId(0));
        }
    }
}
