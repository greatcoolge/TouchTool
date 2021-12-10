package top.bogey.auto_touch.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.lzf.easyfloat.EasyFloat;
import com.lzf.easyfloat.enums.ShowPattern;

import java.util.List;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;
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
import top.bogey.auto_touch.util.CompleteCallback;

@SuppressLint("ViewConstructor")
public class TaskRecordDialog extends FrameLayout implements NodePickerInterface {
    private FloatFragmentRecordBinding binding;
    public final Task task;
    private final CompleteCallback callback;

    private float lastY = 0f;
    private boolean isToBottom = false;
    private boolean isToTop = true;

    public TaskRecordDialog(Context context, Task task, CompleteCallback callback){
        super(context);
        this.task = task;
        this.callback = callback;
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

    @SuppressLint("ClickableViewAccessibility")
    public void initView(MainActivity activity) {
        binding = FloatFragmentRecordBinding.inflate(LayoutInflater.from(getContext()));
        addView(binding.getRoot());
        MainViewModel viewModel = new ViewModelProvider(activity).get(MainViewModel.class);

        RecordActionsRecyclerViewAdapter adapter = new RecordActionsRecyclerViewAdapter(this, task.actions);
        binding.recyclerView.setAdapter(adapter);

        binding.recyclerView.setOnTouchListener((v, event) -> {
            ViewParent parent = getParent();
            if (parent != null){
                switch (event.getAction()){
                    case MotionEvent.ACTION_DOWN:
                        lastY = event.getY();
                        parent.requestDisallowInterceptTouchEvent(true);
                        break;
                    case MotionEvent.ACTION_MOVE:
                        checkPosition(event.getY());
                        if (isToBottom || isToTop){
                            parent.requestDisallowInterceptTouchEvent(false);
                            return false;
                        } else {
                            parent.requestDisallowInterceptTouchEvent(true);
                        }
                        lastY = event.getY();
                        break;
                    case MotionEvent.ACTION_UP:
                    case MotionEvent.ACTION_CANCEL:
                        parent.requestDisallowInterceptTouchEvent(false);
                        break;
                }
            }
            return false;
        });

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
                if (callback != null){
                    callback.onComplete();
                }
            }
            dismiss();
        });
    }

    private void checkPosition(float nowY){
        LinearLayoutManager layoutManager = (LinearLayoutManager)binding.recyclerView.getLayoutManager();
        if (layoutManager != null){
            if (layoutManager.getItemCount() > 3){
                isToTop = false;
                isToBottom =false;
                int first = layoutManager.findFirstCompletelyVisibleItemPosition();
                int last = layoutManager.findLastCompletelyVisibleItemPosition();

                if (layoutManager.getChildCount() > 0){
                    if (last == layoutManager.getItemCount() - 1){
                        if (canScrollVertically(-1) && nowY < lastY){
                            isToBottom = true;
                        }
                    } else if (first == 0){
                        if (canScrollVertically(1) && nowY > lastY){
                            isToTop = true;
                        }
                    }
                }
            } else {
                isToTop = true;
                isToBottom = true;
            }
        }
    }
}
