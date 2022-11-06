package top.bogey.touch_tool.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.widget.FrameLayout;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.DelayAction;
import top.bogey.touch_tool.database.bean.action.TouchAction;
import top.bogey.touch_tool.databinding.FloatPickerPosBinding;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.DouglasPeucker;
import top.bogey.touch_tool.utils.FloatBaseCallback;
import top.bogey.touch_tool.utils.ResultCallback;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;

@SuppressLint("ViewConstructor")
public class QuickRecordFloatView extends FrameLayout implements FloatViewInterface {
    private final List<Behavior> behaviors;
    private final FloatPickerPosBinding binding;

    private List<Point> currPoints = new ArrayList<>();
    private float lastX = 0;
    private float lastY = 0;
    private boolean isDrag = false;

    int[] location = new int[2];

    private final Paint paint;

    private long delayStartTime = 0;
    private long touchStartTime = 0;

    public QuickRecordFloatView(Context context, Task task, ResultCallback callback) {
        super(context);
        behaviors = task.getBehaviors();

        binding = FloatPickerPosBinding.inflate(LayoutInflater.from(getContext()), this, true);

        binding.saveButton.setOnClickListener(v -> {
            if (behaviors.size() > 0) {
                task.setBehaviors(behaviors);
            }
            if (callback != null) {
                callback.onResult(true);
            }
            dismiss();
        });

        paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(DisplayUtils.getAttrColor(getContext(), com.google.android.material.R.attr.colorPrimaryContainer, 0));
        paint.setStrokeWidth(10);
        paint.setStrokeCap(Paint.Cap.ROUND);

        refreshUI();
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        getLocationOnScreen(location);
    }

    private void refreshUI() {
        binding.buttonBox.setVisibility(VISIBLE);
        binding.backButton.setVisibility(GONE);

        if (binding.buttonBox.getWidth() == 0) {
            post(this::refreshUI);
            return;
        }

        Point size = DisplayUtils.getScreenSize(getContext());
        binding.buttonBox.setX((size.x - binding.buttonBox.getWidth()) / 2f);
        binding.buttonBox.setY(size.y - DisplayUtils.dp2px(getContext(), 64) - location[1] - binding.buttonBox.getHeight());
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {
        super.dispatchDraw(canvas);

        if (currPoints.size() >= 2) {
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
    public boolean onTouchEvent(MotionEvent event) {
        float x = event.getRawX();
        float y = event.getRawY();
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                touchStartTime = System.currentTimeMillis();
                if (delayStartTime != 0) {
                    Action delayNode = new DelayAction((int) (touchStartTime - delayStartTime));
                    behaviors.add(new Behavior(delayNode));
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
                TouchAction touchNode = new TouchAction(getContext(), DouglasPeucker.compress(currPoints));
                touchNode.getTimeArea().setTime((int) touchStartTime);
                behaviors.add(new Behavior(touchNode));

                MainAccessibilityService service = MainApplication.getService();
                if (service != null) {
                    EasyFloat.hide(QuickRecordFloatView.class.getCanonicalName());
                    post(() -> service.runGesture(touchNode.getPath(getContext(), false), touchNode.getTimeArea().getRandomTime(), result -> {
                        EasyFloat.show(QuickRecordFloatView.class.getCanonicalName());
                        delayStartTime = System.currentTimeMillis();
                        currPoints = new ArrayList<>();
                        postInvalidate();
                    }));
                }
                break;
        }
        postInvalidate();
        return true;
    }

    @Override
    public void show() {
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
