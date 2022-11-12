package top.bogey.touch_tool.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.ColorAction;
import top.bogey.touch_tool.database.bean.action.DelayAction;
import top.bogey.touch_tool.database.bean.action.ImageAction;
import top.bogey.touch_tool.database.bean.action.SystemAction;
import top.bogey.touch_tool.database.bean.action.TaskAction;
import top.bogey.touch_tool.database.bean.action.TextAction;
import top.bogey.touch_tool.database.bean.action.TouchAction;
import top.bogey.touch_tool.databinding.FloatRecordBinding;
import top.bogey.touch_tool.ui.behavior.BehaviorFloatView;
import top.bogey.touch_tool.ui.setting.SettingSave;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.FloatBaseCallback;
import top.bogey.touch_tool.utils.ResultCallback;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;

@SuppressLint("ViewConstructor")
public class RecordFloatView extends FrameLayout implements FloatViewInterface {
    public final Task task;

    private final FloatRecordBinding binding;
    private final RecordRecyclerViewAdapter adapter;

    private float lastX = 0f;
    private boolean isToLeft = true;
    private boolean isToRight = false;

    private final int delay;

    @SuppressLint("ClickableViewAccessibility")
    public RecordFloatView(@NonNull Context context, Task task, ResultCallback callback) {
        super(context);
        this.task = task;
        delay = SettingSave.getInstance().getActionRecordDelay();

        binding = FloatRecordBinding.inflate(LayoutInflater.from(context), this, true);

        adapter = new RecordRecyclerViewAdapter(task);
        binding.recyclerView.setAdapter(adapter);

        binding.recyclerView.setOnTouchListener((v, event) -> {
            ViewParent parent = getParent();
            if (parent != null) {
                switch (event.getAction()) {
                    case MotionEvent.ACTION_DOWN:
                        lastX = event.getX();
                        parent.requestDisallowInterceptTouchEvent(true);
                        break;
                    case MotionEvent.ACTION_MOVE:
                        checkPosition(binding.recyclerView, event.getX());
                        if (isToRight || isToLeft) {
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
            task.setBehaviors(adapter.behaviors);
            if (callback != null) callback.onResult(true);
            dismiss();
        });

        binding.delayButton.setOnClickListener(v -> addAction(new DelayAction()));
        binding.wordButton.setOnClickListener(v -> addAction(new TextAction()));
        binding.imageButton.setOnClickListener(v -> addAction(new ImageAction()));
        binding.posButton.setOnClickListener(v -> addAction(new TouchAction()));
        binding.colorButton.setOnClickListener(v -> addAction(new ColorAction()));
        binding.keyButton.setOnClickListener(v -> addAction(new SystemAction()));
        binding.taskButton.setOnClickListener(v -> addAction(new TaskAction()));
    }

    @Override
    public void show() {
        List<Behavior> behaviors = task.getBehaviors();
        EasyFloat.with(getContext())
                .setLayout(this)
                .setTag(RecordFloatView.class.getCanonicalName())
                .setDragEnable(true)
                .setGravity(FloatGravity.BOTTOM_CENTER, 0, (behaviors != null && !behaviors.isEmpty()) ? 0 : -DisplayUtils.dp2px(getContext(), 40))
                .setCallback(new FloatBaseCallback())
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(RecordFloatView.class.getCanonicalName());
    }

    private void addAction(Action action) {
        Behavior behavior = new Behavior(action);
        new BehaviorFloatView(getContext(), task, behavior, result -> {
            adapter.addBehavior(behavior);
            if (delay > 0) {
                adapter.addBehavior(new Behavior(new DelayAction(delay)));
            }
        }).show();
    }

    private void checkPosition(RecyclerView view, float nowX) {
        LinearLayoutManager layoutManager = (LinearLayoutManager) view.getLayoutManager();
        if (layoutManager != null) {
            if (layoutManager.getItemCount() > 6) {
                isToLeft = false;
                isToRight = false;
                int first = layoutManager.findFirstCompletelyVisibleItemPosition();
                int last = layoutManager.findLastCompletelyVisibleItemPosition();

                if (layoutManager.getChildCount() > 0) {
                    if (last == layoutManager.getItemCount() - 1) {
                        if (canScrollHorizontally(-1) && nowX < lastX) {
                            isToRight = true;
                        }
                    } else if (first == 0) {
                        if (canScrollHorizontally(1) && nowX > lastX) {
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
