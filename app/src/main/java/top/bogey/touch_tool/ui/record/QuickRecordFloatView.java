package top.bogey.touch_tool.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Point;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.widget.FrameLayout;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.database.bean.Behavior;
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
    private final FloatPickerPosBinding binding;
    private final List<Behavior> behaviors = new ArrayList<>();
    private final Paint paint;
    private final int[] location = new int[2];

    private List<TouchAction.TouchPath> paths = new ArrayList<>();
    private long delayStartTime = 0;
    private long touchStartTime = 0;

    public QuickRecordFloatView(Context context, List<Behavior> baseBehaviors, ResultCallback callback) {
        super(context);
        binding = FloatPickerPosBinding.inflate(LayoutInflater.from(getContext()), this, true);

        binding.saveButton.setOnClickListener(v -> {
            baseBehaviors.addAll(behaviors);
            if (callback != null) {
                callback.onResult(true);
            }
            dismiss();
        });

        binding.backButton.setOnClickListener(v -> dismiss());

        paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(DisplayUtils.getAttrColor(getContext(), com.google.android.material.R.attr.colorPrimaryContainer, 0));
        paint.setStrokeWidth(10);
        paint.setStrokeCap(Paint.Cap.ROUND);
        paint.setStrokeJoin(Paint.Join.ROUND);
        paint.setStyle(Paint.Style.STROKE);

        refreshUI();
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        getLocationOnScreen(location);
    }

    private void refreshUI() {
        binding.buttonBox.setVisibility(VISIBLE);
        binding.backButton.setVisibility(VISIBLE);

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

        for (TouchAction.TouchPath touchPath : paths) {
            List<Point> points = touchPath.getPoints();
            if (points.size() >= 2) {
                Path path = new Path();
                for (Point point : points) {
                    if (path.isEmpty()) path.moveTo(point.x - location[0], point.y - location[1]);
                    else path.lineTo(point.x - location[0], point.y - location[1]);
                }
                canvas.drawPath(path, paint);
            }
            if (points.size() >= 1) {
                Point point = points.get(points.size() - 1);
                canvas.drawCircle(point.x - location[0], point.y - location[1], 5, paint);
            }
        }
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        switch (event.getActionMasked()) {
            case MotionEvent.ACTION_DOWN:
                touchStartTime = System.currentTimeMillis();
                // 两次点击间隔小于50ms算作在发抖
                if (delayStartTime != 0 && touchStartTime - delayStartTime > 50) {
                    Action delayNode = new DelayAction((int) (touchStartTime - delayStartTime));
                    behaviors.add(new Behavior(delayNode));
                }
                paths = new ArrayList<>();
                addNewPath(event);
                break;
            case MotionEvent.ACTION_MOVE:
                for (int i = 0; i < event.getPointerCount(); i++) {
                    int pointerId = event.getPointerId(i);
                    for (TouchAction.TouchPath path : paths) {
                        if (path.getPointerId() == pointerId) {
                            float currX = 0, currY = 0;
                            for (int j = 0; j < event.getHistorySize(); j++) {
                                currX = event.getHistoricalX(i, j) + location[0];
                                currY = event.getHistoricalY(i, j) + location[1];
                            }
                            if (!(currX == 0 && currY == 0)) path.addPoint((int) currX, (int) currY);
                        }
                    }
                }
                break;
            case MotionEvent.ACTION_UP:
                delayStartTime = System.currentTimeMillis();
                long touchTime = delayStartTime - touchStartTime;
                paths.forEach(path -> path.setPoints(DouglasPeucker.compress(path.getPoints())));

                TouchAction touchNode = new TouchAction(getContext(), paths);
                touchNode.getTimeArea().setTime((int) touchTime);
                behaviors.add(new Behavior(touchNode));

                MainAccessibilityService service = MainApplication.getService();
                if (service != null && service.isServiceEnabled()) {
                    EasyFloat.hide(QuickRecordFloatView.class.getCanonicalName());
                    post(() -> service.runGesture(touchNode.getPaths(getContext(), false), touchNode.getTimeArea().getRandomTime(), result -> {
                        EasyFloat.show(QuickRecordFloatView.class.getCanonicalName());
                        delayStartTime = System.currentTimeMillis();
                    }));
                }
                break;
            case MotionEvent.ACTION_POINTER_UP:
                int pointerId = event.getPointerId(event.getActionIndex());
                for (TouchAction.TouchPath path : paths) {
                    if (path.getPointerId() == pointerId) {
                        path.setPointerId(-1);
                    }
                    break;
                }
                break;
            case MotionEvent.ACTION_POINTER_DOWN:
                addNewPath(event);
                break;
        }
        postInvalidate();
        return true;
    }

    private void addNewPath(MotionEvent event) {
        TouchAction.TouchPath path = new TouchAction.TouchPath();
        int pointerId = event.getPointerId(event.getActionIndex());
        path.setPointerId(pointerId);
        int x = (int) event.getX(event.findPointerIndex(pointerId));
        int y = (int) event.getY(event.findPointerIndex(pointerId));
        x = x + location[0];
        y = y + location[1];
        path.addPoint(x, y);
        paths.add(path);
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
