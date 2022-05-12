package top.bogey.auto_touch.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;

import java.util.Collections;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentRecordBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.action.FloatActionEdit;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.easy_float.FloatGravity;
import top.bogey.auto_touch.ui.picker.FloatShowCallback;
import top.bogey.auto_touch.util.CompleteCallback;

@SuppressLint("ViewConstructor")
public class TaskRecordDialog extends FrameLayout {
    private FloatFragmentRecordBinding binding;
    public final Task task;
    private final CompleteCallback callback;
    private RecordActionsRecyclerViewAdapter adapter;

    private float lastY = 0f;
    private boolean isToBottom = false;
    private boolean isToTop = true;

    public TaskRecordDialog(Context context, Task task, CompleteCallback callback){
        super(context);
        this.task = task;
        this.callback = callback;
    }

    public void show(){
        MainActivity activity = MainApplication.getActivity();
        if (activity != null){
            initView(activity);
            EasyFloat.with(activity)
                    .setLayout(this)
                    .setTag(TaskRecordDialog.class.getCanonicalName())
                    .setDragEnable(true)
                    .setGravity(FloatGravity.BOTTOM_CENTER, 0, 0)
                    .setCallback(new FloatShowCallback())
                    .show();
        }
    }

    public void dismiss() {
        EasyFloat.dismiss(TaskRecordDialog.class.getCanonicalName());
    }

    @SuppressLint("ClickableViewAccessibility")
    public void initView(MainActivity activity) {
        binding = FloatFragmentRecordBinding.inflate(LayoutInflater.from(getContext()));
        addView(binding.getRoot());

        adapter = new RecordActionsRecyclerViewAdapter(this, task.getActions());
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

        binding.delayButton.setOnClickListener(v -> addNewAction(NodeType.DELAY));
        binding.wordButton.setOnClickListener(v -> addNewAction(NodeType.TEXT));
        binding.imageButton.setOnClickListener(v -> addNewAction(NodeType.IMAGE));
        binding.posButton.setOnClickListener(v -> addNewAction(NodeType.POS));
        binding.keyButton.setOnClickListener(v -> addNewAction(NodeType.KEY));
        binding.taskButton.setOnClickListener(v -> addNewAction(NodeType.TASK));

        binding.saveButton.setOnClickListener(v -> {
            if (!adapter.actions.isEmpty()){
                task.setActions(adapter.actions);
                if (callback != null){
                    callback.onComplete();
                }
            }
            dismiss();
        });
    }

    private void addNewAction(NodeType type){
        Action action = new Action();
        action.setTargets(Collections.singletonList(new Node(type)));
        new FloatActionEdit(getContext(), task, action, () -> adapter.addAction(action)).show();
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
