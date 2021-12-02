package top.bogey.auto_touch.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.lifecycle.ViewModelProvider;

import com.lzf.easyfloat.EasyFloat;
import com.lzf.easyfloat.enums.ShowPattern;

import java.util.List;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentRecordBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.ActionMode;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.room.bean.SimpleTaskInfo;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.picker.FloatShowCallback;
import top.bogey.auto_touch.ui.picker.ImagePicker;
import top.bogey.auto_touch.ui.picker.KeyPicker;
import top.bogey.auto_touch.ui.picker.NodePickerInterface;
import top.bogey.auto_touch.ui.picker.PosPicker;
import top.bogey.auto_touch.ui.picker.TaskPicker;
import top.bogey.auto_touch.ui.picker.WordPicker;

@SuppressLint("ViewConstructor")
public class TaskRecordDialog extends FrameLayout implements NodePickerInterface {
    public final Task task;

    public TaskRecordDialog(Context context, String pkgName){
        super(context);
        task = new Task();
        task.id = 0;
        task.title = context.getString(R.string.task_default_title);
        task.pkgName = pkgName;
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
                    .setTag(TaskRecordDialog.class.getCanonicalName())
                    .setDragEnable(true)
                    .setGravity(Gravity.BOTTOM | Gravity.CENTER_HORIZONTAL, 0, 0)
                    .registerCallbacks(new FloatShowCallback())
                    .hasEditText(true)
                    .setAnimator(null)
                    .show();
        }
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(TaskRecordDialog.class.getCanonicalName());
    }

    public void initView(MainActivity activity) {
        top.bogey.auto_touch.databinding.FloatFragmentRecordBinding binding = FloatFragmentRecordBinding.inflate(LayoutInflater.from(getContext()));
        addView(binding.getRoot());
        MainViewModel viewModel = new ViewModelProvider(activity).get(MainViewModel.class);

        RecordActionsRecyclerViewAdapter adapter = new RecordActionsRecyclerViewAdapter(this);
        binding.recyclerView.setAdapter(adapter);

        binding.wordButton.setOnClickListener(v -> new WordPicker(getContext(), nodePicker -> {
            WordPicker wordPicker = (WordPicker) nodePicker;
            String key = wordPicker.getKey();
            if (key != null && !key.isEmpty()){
                Action action = new Action();
                action.actionMode = ActionMode.TOUCH;
                action.target = new Node(NodeType.WORD, key);
                adapter.addAction(action);
            }
        }).show(Gravity.START | Gravity.TOP, 0, 0));

        binding.imageButton.setOnClickListener(v -> new ImagePicker(getContext(), nodePicker -> {
            ImagePicker imagePicker = (ImagePicker) nodePicker;
            Bitmap bitmap = imagePicker.getBitmap();
            if (bitmap != null){
                Action action = new Action();
                action.actionMode = ActionMode.TOUCH;
                Node node = new Node();
                node.setImage(bitmap);
                action.target = node;
                adapter.addAction(action);
            }
        }).show(Gravity.START | Gravity.TOP, 0, 0));

        binding.posButton.setOnClickListener(v -> new PosPicker(getContext(), nodePicker -> {
            PosPicker posPicker = (PosPicker) nodePicker;
            List<Pos> posList = posPicker.getPosList();
            Action action = new Action();
            action.actionMode = ActionMode.TOUCH;
            Node node = new Node();
            node.setPoses(posList);
            action.target = node;
            adapter.addAction(action);
        }, null).show(Gravity.START | Gravity.TOP, 0, 0));

        binding.keyButton.setOnClickListener(v -> new KeyPicker(getContext(), nodePicker -> {
            KeyPicker keyPicker = (KeyPicker) nodePicker;
            SimpleTaskInfo taskInfo = keyPicker.getTaskInfo();
            if (taskInfo != null){
                Action action = new Action();
                action.actionMode = ActionMode.KEY;
                Node node = new Node();
                node.setTask(taskInfo);
                action.target = node;
                adapter.addAction(action);
            }
        }).show(Gravity.START | Gravity.TOP, 0, 0));

        binding.taskButton.setOnClickListener(v -> new TaskPicker(getContext(), nodePicker -> {
            TaskPicker taskPicker = (TaskPicker) nodePicker;
            SimpleTaskInfo taskInfo = taskPicker.getTaskInfo();
            if (taskInfo != null){
                Action action = new Action();
                action.actionMode = ActionMode.TASK;
                Node node = new Node();
                node.setTask(taskInfo);
                action.target = node;
                adapter.addAction(action);
            }
        }, task.pkgName).show(Gravity.START | Gravity.TOP, 0, 0));

        binding.saveButton.setOnClickListener(v -> {
            if (!adapter.actions.isEmpty()){
                task.actions = adapter.actions;
                viewModel.saveTask(task);
            }
            dismiss();
        });
    }
}
