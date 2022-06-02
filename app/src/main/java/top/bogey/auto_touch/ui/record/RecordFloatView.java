package top.bogey.auto_touch.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.Collections;
import java.util.List;

import top.bogey.auto_touch.databinding.FloatRecordBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.node.DelayNode;
import top.bogey.auto_touch.room.bean.node.ImageNode;
import top.bogey.auto_touch.room.bean.node.KeyNode;
import top.bogey.auto_touch.room.bean.node.Node;
import top.bogey.auto_touch.room.bean.node.TaskNode;
import top.bogey.auto_touch.room.bean.node.TextNode;
import top.bogey.auto_touch.room.bean.node.TimeArea;
import top.bogey.auto_touch.room.bean.node.TouchNode;
import top.bogey.auto_touch.ui.actions.ActionFloatView;
import top.bogey.auto_touch.utils.DisplayUtils;
import top.bogey.auto_touch.utils.FloatBaseCallback;
import top.bogey.auto_touch.utils.ResultCallback;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;
import top.bogey.auto_touch.utils.easy_float.FloatGravity;
import top.bogey.auto_touch.utils.easy_float.FloatViewInterface;

@SuppressLint("ViewConstructor")
public class RecordFloatView extends FrameLayout implements FloatViewInterface {
    public final Task task;

    private final FloatRecordBinding binding;
    private final RecordRecyclerViewAdapter adapter;

    private float lastX = 0f;
    private boolean isToLeft = true;
    private boolean isToRight = false;

    @SuppressLint("ClickableViewAccessibility")
    public RecordFloatView(@NonNull Context context, Task task, ResultCallback callback) {
        super(context);
        this.task = task;

        binding = FloatRecordBinding.inflate(LayoutInflater.from(context), this, true);

        adapter = new RecordRecyclerViewAdapter(task);
        binding.recyclerView.setAdapter(adapter);

        binding.recyclerView.setOnTouchListener((v, event) -> {
            ViewParent parent = getParent();
            if (parent != null){
                switch (event.getAction()){
                    case MotionEvent.ACTION_DOWN:
                        lastX = event.getX();
                        parent.requestDisallowInterceptTouchEvent(true);
                        break;
                    case MotionEvent.ACTION_MOVE:
                        checkPosition(binding.recyclerView, event.getX());
                        if (isToRight || isToLeft){
                            parent.requestDisallowInterceptTouchEvent(false);
                            return false;
                        } else {
                            parent.requestDisallowInterceptTouchEvent(true);
                        }
                        lastX = event.getX();
                        break;
                    case MotionEvent.ACTION_UP:
                    case MotionEvent.ACTION_CANCEL:
                        parent.requestDisallowInterceptTouchEvent(false);
                        break;
                }
            }
            return false;
        });

        binding.saveButton.setOnClickListener(v -> {
            if (!adapter.actions.isEmpty()){
                task.setActions(adapter.actions);
                if (callback != null) callback.onResult(true);
            }
            dismiss();
        });

        binding.delayButton.setOnClickListener(v -> addAction(new DelayNode(new TimeArea(1000, 1000))));
        binding.wordButton.setOnClickListener(v -> addAction(new TextNode("")));
        binding.imageButton.setOnClickListener(v -> addAction(new ImageNode(new ImageNode.ImageInfo(95))));
        binding.posButton.setOnClickListener(v -> addAction(new TouchNode("")));
        binding.keyButton.setOnClickListener(v -> addAction(new KeyNode(1)));
        binding.taskButton.setOnClickListener(v -> addAction(new TaskNode(null)));

    }

    @Override
    public void show() {
        List<Action> actions = task.getActions();
        EasyFloat.with(getContext())
                .setLayout(this)
                .setTag(RecordFloatView.class.getCanonicalName())
                .setDragEnable(true)
                .setGravity(FloatGravity.BOTTOM_CENTER, 0, (actions != null && !actions.isEmpty()) ? 0 : -DisplayUtils.dp2px(getContext(), 40))
                .setCallback(new FloatBaseCallback())
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(RecordFloatView.class.getCanonicalName());
    }

    private void addAction(Node node){
        Action action = new Action();
        action.setTargets(Collections.singletonList(node));
        new ActionFloatView(getContext(), task, action, result -> {
            adapter.addAction(action);
            binding.recyclerView.scrollToPosition(adapter.getItemCount() - 1);
        }).show();
    }

    private void checkPosition(RecyclerView view, float nowX){
        LinearLayoutManager layoutManager = (LinearLayoutManager)view.getLayoutManager();
        if (layoutManager != null){
            if (layoutManager.getItemCount() > 6){
                isToLeft = false;
                isToRight = false;
                int first = layoutManager.findFirstCompletelyVisibleItemPosition();
                int last = layoutManager.findLastCompletelyVisibleItemPosition();

                if (layoutManager.getChildCount() > 0){
                    if (last == layoutManager.getItemCount() - 1){
                        if (canScrollHorizontally(-1) && nowX < lastX){
                            isToRight = true;
                        }
                    } else if (first == 0){
                        if (canScrollHorizontally(1) && nowX > lastX){
                            isToLeft = true;
                        }
                    }
                }
            } else {
                isToLeft = true;
                isToRight = true;
            }
        }
    }
}
