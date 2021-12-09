package top.bogey.auto_touch.ui.action;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.text.Editable;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.FrameLayout;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.lzf.easyfloat.EasyFloat;
import com.lzf.easyfloat.enums.ShowPattern;

import java.util.List;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentActionEditBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.ActionMode;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.room.bean.SimpleTaskInfo;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.picker.FloatActionEditShowCallback;
import top.bogey.auto_touch.ui.picker.ImagePicker;
import top.bogey.auto_touch.ui.picker.NodePickerInterface;
import top.bogey.auto_touch.ui.picker.PosPicker;
import top.bogey.auto_touch.ui.picker.WordPicker;
import top.bogey.auto_touch.util.CompleteCallback;

@SuppressLint("ViewConstructor")
public class FloatActionEdit extends FrameLayout implements NodePickerInterface {
    private FloatFragmentActionEditBinding binding;
    private MainViewModel viewModel;
    private final Task task;
    private final Action action;
    private final CompleteCallback callback;

    private ArrayAdapter<SimpleTaskInfo> targetArrayAdapter;
    private ArrayAdapter<SimpleTaskInfo> targetTypeArrayAdapter;
    private ArrayAdapter<SimpleTaskInfo> stopTypeArrayAdapter;

    private int delayScale = 1;
    private int intervalScale = 1;
    private int timeScale = 1;

    private final Node target;
    private SimpleTaskInfo taskInfo;
    private final Node stop;
    private ActionMode mode = ActionMode.NULL;

    public FloatActionEdit(Context context, @NonNull Task task, @NonNull Action action, CompleteCallback callback){
        super(context);
        this.task = task;
        this.action = action;
        this.callback = callback;
        if (action.target != null) target = action.target.clone();
        else target = new Node(NodeType.WORD);
        if (action.stop != null) stop = action.stop.clone();
        else stop = new Node(NodeType.NULL);
    }

    @Override
    public void show(int gravity, int x, int y) {
        show();
    }

    public void show(){
        MainActivity activity = MainApplication.getActivity();
        if (activity != null){
            initView(activity);
            EasyFloat.with(activity)
                    .setLayout(this)
                    .setShowPattern(ShowPattern.ALL_TIME)
                    .setTag(FloatActionEdit.class.getCanonicalName())
                    .setDragEnable(true)
                    .setGravity(Gravity.TOP | Gravity.CENTER_HORIZONTAL, 0, 0)
                    .registerCallbacks(new FloatActionEditShowCallback())
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

        KeysRecyclerViewAdapter adapter = new KeysRecyclerViewAdapter(this, action.keys);
        binding.keysBox.setAdapter(adapter);

        targetArrayAdapter = new ArrayAdapter<>(getContext(), R.layout.float_fragment_action_edit_picker);
        binding.targetSpinner.setAdapter(targetArrayAdapter);
        binding.targetSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                taskInfo = targetArrayAdapter.getItem(position);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) { }
        });

        String[] strings = getResources().getStringArray(R.array.node_type);
        targetTypeArrayAdapter = new ArrayAdapter<>(getContext(), R.layout.float_fragment_action_edit_picker);
        NodeType[] nodeTypes = {NodeType.WORD, NodeType.IMAGE, NodeType.POS};
        for (NodeType nodeType : nodeTypes) {
            targetTypeArrayAdapter.add(new SimpleTaskInfo(nodeType.ordinal(), strings[nodeType.ordinal()]));
        }
        binding.targetTypeSpinner.setAdapter(targetTypeArrayAdapter);
        binding.targetTypeSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if (mode == ActionMode.TOUCH){
                    SimpleTaskInfo item = targetTypeArrayAdapter.getItem(position);
                    NodeType value = NodeType.values()[item.id];
                    switch (value){
                        case WORD:
                            binding.targetEdit.setVisibility(VISIBLE);
                            binding.targetImage.setVisibility(GONE);
                            binding.targetEdit.setEnabled(true);
                            binding.targetPicker.setIconResource(R.drawable.text);
                            if (target.type != NodeType.WORD) target.setWord("");
                            binding.targetEdit.setText(target.getWord());
                            break;
                        case IMAGE:
                            binding.targetEdit.setVisibility(GONE);
                            binding.targetImage.setVisibility(VISIBLE);
                            binding.targetPicker.setIconResource(R.drawable.image);
                            if (target.type != NodeType.IMAGE) target.setImage(null);
                            if (target.getImage() != null) binding.targetImage.setImageBitmap(target.getImage());
                            break;
                        case POS:
                            binding.targetEdit.setVisibility(VISIBLE);
                            binding.targetImage.setVisibility(GONE);
                            binding.targetEdit.setEnabled(false);
                            binding.targetPicker.setIconResource(R.drawable.pos);
                            if (target.type != NodeType.POS) target.setPoses((List<Pos>) null);
                            binding.targetEdit.setText(target.getWord());
                            break;
                    }
                }
            }
            @Override
            public void onNothingSelected(AdapterView<?> parent) { }
        });
        selectSpinner(binding.targetTypeSpinner, target.type.ordinal());

        stopTypeArrayAdapter = new ArrayAdapter<>(getContext(), R.layout.float_fragment_action_edit_picker);
        nodeTypes = new NodeType[]{NodeType.NULL, NodeType.NUMBER, NodeType.WORD, NodeType.IMAGE};
        for (NodeType nodeType : nodeTypes) {
            stopTypeArrayAdapter.add(new SimpleTaskInfo(nodeType.ordinal(), strings[nodeType.ordinal()]));
        }
        binding.stopTypeSpinner.setAdapter(stopTypeArrayAdapter);
        binding.stopTypeSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                SimpleTaskInfo item = stopTypeArrayAdapter.getItem(position);
                NodeType value = NodeType.values()[item.id];
                switch (value){
                    case NULL:
                        binding.stopEdit.setVisibility(VISIBLE);
                        binding.stopImage.setVisibility(GONE);
                        binding.stopEdit.setEnabled(false);
                        binding.stopPicker.setVisibility(GONE);
                        binding.stopEdit.setInputType(EditorInfo.TYPE_CLASS_TEXT);
                        binding.stopEdit.setText(item.title);
                        stop.setNull();
                        break;
                    case NUMBER:
                    case WORD:
                        binding.stopEdit.setVisibility(VISIBLE);
                        binding.stopImage.setVisibility(GONE);
                        binding.stopEdit.setEnabled(true);
                        if (value == NodeType.NUMBER){
                            binding.stopPicker.setVisibility(GONE);
                            if (stop.type != NodeType.NUMBER) stop.setNumber(1);
                            binding.stopEdit.setInputType(EditorInfo.TYPE_CLASS_NUMBER);
                            binding.stopEdit.setText(String.valueOf(stop.getNumber()));
                        } else {
                            binding.stopPicker.setVisibility(VISIBLE);
                            binding.stopPicker.setIconResource(R.drawable.text);
                            if (stop.type != NodeType.WORD) stop.setWord("");
                            binding.stopEdit.setInputType(EditorInfo.TYPE_CLASS_TEXT);
                            binding.stopEdit.setText(stop.getWord());
                        }
                        break;
                    case IMAGE:
                        binding.stopEdit.setVisibility(GONE);
                        binding.stopImage.setVisibility(VISIBLE);
                        binding.stopPicker.setVisibility(VISIBLE);
                        binding.stopPicker.setIconResource(R.drawable.image);
                        if (stop.type != NodeType.IMAGE) stop.setImage(null);
                        if (stop.getImage() != null) binding.stopImage.setImageBitmap(stop.getImage());
                        break;
                }
            }
            @Override
            public void onNothingSelected(AdapterView<?> parent) { }
        });
        selectSpinner(binding.stopTypeSpinner, stop.type.ordinal());

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

        binding.modeGroup.setOnCheckedChangeListener((group, checkedId) -> {
            int index = group.indexOfChild(group.findViewById(checkedId));
            changeMode(ActionMode.values()[index + 1]);
        });
        binding.modeGroup.check(binding.modeGroup.getChildAt(action.actionMode.ordinal() - 1).getId());

        binding.delayEdit.setText(String.valueOf(action.delay / delayScale));
        binding.timeEdit.setText(String.valueOf(action.time / timeScale));
        binding.intervalEdit.setText(String.valueOf(action.interval / intervalScale));
        binding.timesEdit.setText(String.valueOf(action.times));

        binding.targetPicker.setOnClickListener(v -> {
            switch (target.type){
                case WORD:
                    new WordPicker(getContext(), nodePicker -> {
                        WordPicker wordPicker = (WordPicker) nodePicker;
                        String key = wordPicker.getKey();
                        binding.targetEdit.setText(key);
                    }).show(Gravity.START | Gravity.TOP, 0, 0);
                    break;
                case IMAGE:
                    new ImagePicker(getContext(), nodePicker -> {
                        ImagePicker imagePicker = (ImagePicker) nodePicker;
                        Bitmap bitmap = imagePicker.getBitmap();
                        binding.targetImage.setImageBitmap(bitmap);
                    }).show(Gravity.START | Gravity.TOP, 0, 0);
                    break;
                case POS:
                    Editable text = binding.targetEdit.getText();
                    List<Pos> posList = null;
                    if (text != null && text.length() > 0){
                        posList = new Gson().fromJson(text.toString(), new TypeToken<List<Pos>>(){}.getType());
                    }
                    new PosPicker(getContext(), nodePicker -> {
                        PosPicker posPicker = (PosPicker) nodePicker;
                        String json = new Gson().toJson(posPicker.getPosList());
                        binding.targetEdit.setText(json);
                    }, posList).show(Gravity.START | Gravity.TOP, 0, 0);
                    break;
            }
        });

        binding.stopPicker.setOnClickListener(v -> {
            switch (stop.type){
                case WORD:
                    new WordPicker(getContext(), nodePicker -> {
                        WordPicker wordPicker = (WordPicker) nodePicker;
                        String key = wordPicker.getKey();
                        binding.stopEdit.setText(key);
                    }).show(Gravity.START | Gravity.TOP, 0, 0);
                    break;
                case IMAGE:
                    new ImagePicker(getContext(), nodePicker -> {
                        ImagePicker imagePicker = (ImagePicker) nodePicker;
                        Bitmap bitmap = imagePicker.getBitmap();
                        binding.stopImage.setImageBitmap(bitmap);
                    }).show(Gravity.START | Gravity.TOP, 0, 0);
                    break;
            }
        });

        binding.doKeysImagePicker.setOnClickListener(v -> adapter.addNewNode(NodeType.IMAGE));
        binding.doKeysTextPicker.setOnClickListener(v -> adapter.addNewNode(NodeType.WORD));

        binding.closeButton.setOnClickListener(v -> dismiss());

        binding.saveButton.setOnClickListener(v -> {
            String delayString = String.valueOf(binding.delayEdit.getText());
            String timeString = String.valueOf(binding.timeEdit.getText());
            String intervalString = String.valueOf(binding.intervalEdit.getText());
            String timesString = String.valueOf(binding.timesEdit.getText());
            if (delayString.isEmpty() || timeString.isEmpty() || intervalString.isEmpty() || timesString.isEmpty()){
                Toast.makeText(getContext(), R.string.exist_empty_config, Toast.LENGTH_LONG).show();
                return;
            }

            int delay = Integer.parseInt(delayString) * delayScale;
            int time = Integer.parseInt(timeString) * timeScale;
            int interval = Integer.parseInt(intervalString) * intervalScale;
            int times = Integer.parseInt(timesString);
            if (!action.checkTimeSafe(delay, time, interval, times)){
                Toast.makeText(getContext(), R.string.action_timeout, Toast.LENGTH_LONG).show();
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
                Toast.makeText(getContext(), R.string.empty_target, Toast.LENGTH_LONG).show();
                return;
            }

            switch (stop.type) {
                case NUMBER:
                case WORD:
                    String stopWord = String.valueOf(binding.stopEdit.getText());
                    if (stopWord.isEmpty()) flag = true;
                    if (stop.type == NodeType.NUMBER){
                        stop.setNumber(Integer.parseInt(stopWord));
                    } else {
                        stop.setWord(stopWord);
                    }
                    break;
                case IMAGE:
                    if (binding.stopImage.getDrawable() == null){
                        flag = true;
                        break;
                    }
                    Bitmap bitmap = ((BitmapDrawable) binding.stopImage.getDrawable()).getBitmap();
                    stop.setImage(bitmap);
                    break;
            }
            if (flag){
                Toast.makeText(getContext(), R.string.empty_stop, Toast.LENGTH_LONG).show();
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
    }

    private void changeMode(ActionMode mode){
        if (this.mode == mode) return;
        this.mode = mode;
        switch (mode) {
            case TOUCH:
                binding.targetGroup.setVisibility(View.VISIBLE);
                binding.targetSpinner.setVisibility(View.GONE);
                selectSpinner(binding.targetTypeSpinner, target.type.ordinal());
                break;
            case KEY:
            case TASK:
                binding.targetGroup.setVisibility(View.INVISIBLE);
                binding.targetEdit.setVisibility(View.GONE);
                binding.targetImage.setVisibility(View.GONE);
                binding.targetSpinner.setVisibility(View.VISIBLE);

                targetArrayAdapter.clear();
                if (mode == ActionMode.KEY){
                    String[] strings = getResources().getStringArray(R.array.keys);
                    for (int i = 0; i < strings.length; i++) {
                        targetArrayAdapter.add(new SimpleTaskInfo(i + 1, strings[i]));
                    }
                } else {
                    List<Task> tasks = viewModel.getTasksByPackageName(task.pkgName);
                    for (Task task : tasks) {
                        if (task.id != this.task.id && task.taskStatus != TaskStatus.AUTO){
                            targetArrayAdapter.add(new SimpleTaskInfo(task.id, task.title));
                        }
                    }
                }
                targetArrayAdapter.notifyDataSetChanged();
                if (target.type == NodeType.TASK && !target.getWord().isEmpty()){
                    taskInfo = target.getTask();
                } else {
                    taskInfo = null;
                }
                target.setTask(null);
                selectSpinner(binding.targetSpinner, taskInfo != null ? taskInfo.id : -1);
                break;
        }
    }

    private void selectSpinner(@NonNull Spinner spinner, int id){
        ArrayAdapter<SimpleTaskInfo> spinnerAdapter = (ArrayAdapter<SimpleTaskInfo>) spinner.getAdapter();
        for (int i = 0; i < spinnerAdapter.getCount(); i++) {
            SimpleTaskInfo item = spinnerAdapter.getItem(i);
            if (item.id == id) {
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
