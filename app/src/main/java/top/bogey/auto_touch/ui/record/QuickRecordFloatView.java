package top.bogey.auto_touch.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.widget.FrameLayout;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatPickerPosBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.node.DelayNode;
import top.bogey.auto_touch.room.bean.node.Node;
import top.bogey.auto_touch.room.bean.node.NullNode;
import top.bogey.auto_touch.room.bean.node.TimeArea;
import top.bogey.auto_touch.room.bean.node.TouchNode;
import top.bogey.auto_touch.utils.DisplayUtils;
import top.bogey.auto_touch.utils.DouglasPeucker;
import top.bogey.auto_touch.utils.FloatBaseCallback;
import top.bogey.auto_touch.utils.ResultCallback;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;
import top.bogey.auto_touch.utils.easy_float.FloatViewInterface;

@SuppressLint("ViewConstructor")
public class QuickRecordFloatView extends FrameLayout implements FloatViewInterface {
    private final List<Action> actions;

    private final List<Point> currPoints = new ArrayList<>();
    private float lastX = 0;
    private float lastY = 0;
    private boolean isDrag = false;
    private boolean addEnable = true;

    int[] location = new int[2];

    private final Paint paint;

    private long delayStartTime = 0;
    private long touchStartTime = 0;

    public QuickRecordFloatView(Context context, Task task, ResultCallback callback){
        super(context);
        actions = task.getActions();

        FloatPickerPosBinding binding = FloatPickerPosBinding.inflate(LayoutInflater.from(getContext()), this, true);

        binding.saveButton.setOnClickListener(v -> {
            addEnable = false;
            if (actions.size() > 0){
                task.setActions(actions);
            }
            if (callback != null){
                callback.onResult(true);
            }
            dismiss();
        });

        paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(DisplayUtils.getAttrColor(getContext(), com.google.android.material.R.attr.colorPrimaryContainer, 0));
        paint.setStrokeWidth(10);
        paint.setStrokeCap(Paint.Cap.ROUND);
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        getLocationOnScreen(location);
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {
        super.dispatchDraw(canvas);

        if (currPoints.size() >= 2){
            for (int i = 0; i < currPoints.size() - 1; i++) {
                canvas.drawLine(currPoints.get(i).x, currPoints.get(i).y - location[1], currPoints.get(i + 1).x, currPoints.get(i + 1).y - location[1], paint);
            }
        }

        if (currPoints.size() >= 1) {
            Point point = currPoints.get(currPoints.size() - 1);
            canvas.drawCircle(point.x, point.y - location[1], 12, paint);
        }
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event){
        float x = event.getRawX();
        float y = event.getRawY();
        switch (event.getAction()){
            case MotionEvent.ACTION_DOWN:
                touchStartTime = System.currentTimeMillis();
                if (delayStartTime != 0){
                    Node delayNode = new DelayNode(new TimeArea((int) (touchStartTime - delayStartTime)));
                    Action action = new Action();
                    action.setTargets(new ArrayList<>(Collections.singletonList(delayNode)));
                    action.setCondition(new NullNode());
                    actions.add(action);
                    delayStartTime = touchStartTime;
                }
                lastX = x;
                lastY = y;
                isDrag = false;
                currPoints.clear();
                currPoints.add(new Point((int) x, (int) y));
                break;
            case MotionEvent.ACTION_MOVE:
                float dx = x - lastX;
                float dy = y - lastY;
                if (!isDrag && dx * dx + dy * dy < 81) return true;
                isDrag = true;
                currPoints.add(new Point((int) x, (int) y));
                lastX = x;
                lastY = y;
                break;
            case MotionEvent.ACTION_UP:
                isDrag = false;
                touchStartTime = System.currentTimeMillis() - touchStartTime;
                addEnable = true;
                postDelayed(() -> {
                    if (addEnable){
                        TouchNode touchNode = new TouchNode(null);
                        touchNode.setValue(getContext(), DouglasPeucker.compress(currPoints));
                        touchNode.getTimeArea().setTime((int) touchStartTime);
                        Action action = new Action();
                        action.setTargets(new ArrayList<>(Collections.singletonList(touchNode)));
                        action.setCondition(new NullNode());
                        actions.add(action);

                        MainAccessibilityService service = MainApplication.getService();
                        if (service != null){
                            EasyFloat.hide(QuickRecordFloatView.class.getCanonicalName());
                            post(() -> service.runGesture(touchNode.getPath(getContext()), touchNode.getTimeArea().getRandomTime(), result -> {
                                EasyFloat.show(QuickRecordFloatView.class.getCanonicalName());
                                delayStartTime = System.currentTimeMillis();
                                currPoints.clear();
                                postInvalidate();
                            }));
                        }
                    }
                }, 50);
                break;
        }
        postInvalidate();
        return true;
    }

    @Override
    public void show(){
        EasyFloat.with(getContext())
                .setLayout(this)
                .setTag(QuickRecordFloatView.class.getCanonicalName())
                .setDragEnable(false)
                .setMatch(true, true)
                .setCallback(new FloatBaseCallback())
                .setAnimator(null)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(QuickRecordFloatView.class.getCanonicalName());
    }
}
