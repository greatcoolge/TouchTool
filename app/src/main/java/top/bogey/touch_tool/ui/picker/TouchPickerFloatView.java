package top.bogey.touch_tool.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.action.TouchAction;
import top.bogey.touch_tool.databinding.FloatPickerPosBinding;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.DouglasPeucker;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;

@SuppressLint("ViewConstructor")
public class TouchPickerFloatView extends BasePickerFloatView {
    private FloatGravity gravity = FloatGravity.TOP_LEFT;

    private final FloatPickerPosBinding binding;

    private final List<TouchAction.TouchPath> paths = new ArrayList<>();
    private float lastX = 0;
    private float lastY = 0;

    int[] location = new int[2];

    private final Paint paint;
    private final Rect realArea = new Rect();
    private boolean isMarked;

    private final int padding;
    private boolean isInit = true;
    private boolean isChanged = false;

    private boolean isClick = false;

    public TouchPickerFloatView(@NonNull Context context, PickerCallback pickerCallback, TouchAction touchAction) {
        super(context, pickerCallback);

        binding = FloatPickerPosBinding.inflate(LayoutInflater.from(context), this, true);

        if (touchAction != null) {
            paths.addAll(touchAction.getPaths(context));
        }
        isMarked = paths.size() > 0;

        binding.saveButton.setOnClickListener(v -> {
            pickerCallback.onComplete(this);
            dismiss();
        });

        binding.backButton.setOnClickListener(v -> dismiss());

        binding.topLeftButton.setOnClickListener(v -> refreshGravityButton(FloatGravity.TOP_LEFT));
        binding.topRightButton.setOnClickListener(v -> refreshGravityButton(FloatGravity.TOP_RIGHT));
        binding.bottomLeftButton.setOnClickListener(v -> refreshGravityButton(FloatGravity.BOTTOM_LEFT));
        binding.bottomRightButton.setOnClickListener(v -> refreshGravityButton(FloatGravity.BOTTOM_RIGHT));

        paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(DisplayUtils.getAttrColor(getContext(), com.google.android.material.R.attr.colorPrimaryContainer, 0));
        paint.setStrokeWidth(10);
        paint.setStrokeCap(Paint.Cap.ROUND);
        paint.setStrokeJoin(Paint.Join.ROUND);
        paint.setStyle(Paint.Style.STROKE);

        padding = DisplayUtils.dp2px(context, 20);

        refreshGravityButton(touchAction == null ? FloatGravity.TOP_LEFT : touchAction.getGravity());
    }

    @SuppressLint("DrawAllocation")
    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        getLocationOnScreen(location);
        if (isInit && isMarked) refreshUI();
        isInit = false;
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

    public TouchAction getTouchAction() {
        return new TouchAction(getContext(), gravity, getGravityPoint(), getGravityPaths());
    }

    private Point getGravityPoint() {
        Point size = DisplayUtils.getScreenSize(getContext());
        switch (gravity) {
            case TOP_LEFT:
                return new Point(realArea.left, realArea.top);
            case TOP_RIGHT:
                return new Point(realArea.right - size.x, realArea.top);
            case BOTTOM_LEFT:
                return new Point(realArea.left, realArea.bottom - size.y);
            case BOTTOM_RIGHT:
                return new Point(realArea.right - size.x, realArea.bottom - size.y);
        }
        return new Point(0, 0);
    }

    private Point getScreenGravityPoint() {
        switch (gravity) {
            case TOP_LEFT:
                return new Point(realArea.left, realArea.top);
            case TOP_RIGHT:
                return new Point(realArea.right, realArea.top);
            case BOTTOM_LEFT:
                return new Point(realArea.left, realArea.bottom);
            case BOTTOM_RIGHT:
                return new Point(realArea.right, realArea.bottom);
        }
        return new Point();
    }

    private List<TouchAction.TouchPath> getGravityPaths() {
        Point gravityPoint = getScreenGravityPoint();
        List<TouchAction.TouchPath> paths = new ArrayList<>();
        for (TouchAction.TouchPath path : this.paths) {
            TouchAction.TouchPath touchPath = AppUtils.copy(path);
            touchPath.offset(-gravityPoint.x, -gravityPoint.y);
            if (isChanged) touchPath.setPoints(DouglasPeucker.compress(touchPath.getPoints()));
            paths.add(touchPath);
        }
        return paths;
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        float x = event.getRawX();
        float y = event.getRawY();
        switch (event.getActionMasked()) {
            case MotionEvent.ACTION_DOWN:
                boolean flag = true;
                if (isMarked) {
                    int[] location = new int[2];
                    binding.markBox.getLocationOnScreen(location);
                    Rect rect = new Rect(location[0], location[1], location[0] + binding.markBox.getWidth(), location[1] + binding.markBox.getHeight());
                    if (rect.contains((int) x, (int) y)) {
                        flag = false;
                        lastX = x;
                        lastY = y;
                    }
                }
                if (flag) {
                    isMarked = false;
                    paths.clear();
                    isChanged = true;
                    addNewPath(event);
                }
                break;
            case MotionEvent.ACTION_MOVE:
                if (isMarked) {
                    float dx = x - lastX;
                    float dy = y - lastY;
                    for (TouchAction.TouchPath path : paths) {
                        path.offset((int) dx, (int) dy);
                    }
                    lastX = x;
                    lastY = y;
                } else {
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
                }
                break;
            case MotionEvent.ACTION_UP:
                if (isMarked) {
                    if (isClick) {
                        paths.forEach(TouchAction.TouchPath::toLine);
                    } else {
                        isClick = true;
                        postDelayed(() -> isClick = false, 300);
                    }
                } else {
                    isMarked = true;
                    for (TouchAction.TouchPath path : paths) {
                        path.setPointerId(-1);
                    }
                }
                break;
            case MotionEvent.ACTION_POINTER_UP:
                if (!isMarked) {
                    int pointerId = event.getPointerId(event.getActionIndex());
                    for (TouchAction.TouchPath path : paths) {
                        if (path.getPointerId() == pointerId) {
                            path.setPointerId(-1);
                        }
                        break;
                    }
                }
                break;
            case MotionEvent.ACTION_POINTER_DOWN:
                if (!isMarked) addNewPath(event);
                break;
        }
        refreshUI();
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

    private void refreshUI() {
        for (TouchAction.TouchPath path : paths) {
            Rect rect = DisplayUtils.calculatePointArea(path.getPoints());
            if (paths.indexOf(path) == 0) realArea.set(rect);
            else {
                realArea.left = Math.min(rect.left, realArea.left);
                realArea.right = Math.max(rect.right, realArea.right);
                realArea.top = Math.min(rect.top, realArea.top);
                realArea.bottom = Math.max(rect.bottom, realArea.bottom);
            }
        }

        Rect markArea = new Rect(realArea);
        Point size = DisplayUtils.getScreenSize(getContext());
        if (markArea.left < location[0] + padding * 2) markArea.left = location[0] + padding * 2;
        if (markArea.top < location[1] + padding * 2) markArea.top = location[1] + padding * 2;
        if (markArea.right > size.x - padding * 2) markArea.right = size.x - padding * 2;
        if (markArea.bottom > size.y - padding * 2) markArea.bottom = size.y - padding * 2;
        markArea.sort();

        binding.markBox.setVisibility(isMarked ? VISIBLE : INVISIBLE);
        binding.buttonBox.setVisibility(isMarked ? VISIBLE : INVISIBLE);
        if (isMarked) {
            ViewGroup.LayoutParams params = binding.markBox.getLayoutParams();
            params.width = markArea.width() + 2 * padding;
            params.height = markArea.height() + 2 * padding;
            binding.markBox.setLayoutParams(params);

            binding.markBox.setX(markArea.left - padding - location[0]);
            binding.markBox.setY(markArea.top - padding - location[1]);

            binding.topRightButton.setX(params.width - binding.topRightButton.getWidth());
            binding.bottomLeftButton.setY(params.height - binding.bottomLeftButton.getHeight());
            binding.bottomRightButton.setX(params.width - binding.bottomRightButton.getWidth());
            binding.bottomRightButton.setY(params.height - binding.bottomRightButton.getHeight());

            binding.buttonBox.setX(markArea.left + ((float) markArea.width() - binding.buttonBox.getWidth()) / 2 - location[0]);
            if (markArea.bottom + padding * 2 + binding.buttonBox.getHeight() > binding.getRoot().getHeight()) {
                binding.buttonBox.setY(markArea.top - padding * 2 - binding.buttonBox.getHeight() - location[1]);
            } else {
                binding.buttonBox.setY(markArea.bottom + padding * 2 - location[1]);
            }
        }
        postInvalidate();
        if (binding.topRightButton.getWidth() == 0) {
            post(this::refreshUI);
        }
    }

    private void refreshGravityButton(FloatGravity gravity) {
        this.gravity = gravity;
        binding.topLeftButton.setIconResource(R.drawable.icon_radio_checked);
        binding.topRightButton.setIconResource(R.drawable.icon_radio_checked);
        binding.bottomLeftButton.setIconResource(R.drawable.icon_radio_checked);
        binding.bottomRightButton.setIconResource(R.drawable.icon_radio_checked);
        switch (gravity) {
            case TOP_LEFT:
                binding.topLeftButton.setIconResource(R.drawable.icon_radio_unchecked);
                break;
            case TOP_RIGHT:
                binding.topRightButton.setIconResource(R.drawable.icon_radio_unchecked);
                break;
            case BOTTOM_LEFT:
                binding.bottomLeftButton.setIconResource(R.drawable.icon_radio_unchecked);
                break;
            case BOTTOM_RIGHT:
                binding.bottomRightButton.setIconResource(R.drawable.icon_radio_unchecked);
                break;
        }
    }
}
