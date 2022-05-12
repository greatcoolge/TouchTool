package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.view.LayoutInflater;
import android.view.MotionEvent;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentPickerPosBinding;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.DouglasPeucker;

@SuppressLint("ViewConstructor")
public class PosPicker extends NodePicker{
    private final List<Pos> poses;
    private final List<Point> points = new ArrayList<>();
    private final List<Point> currPoints = new ArrayList<>();
    private float lastX = 0;
    private float lastY = 0;
    private boolean isDrag = false;

    int[] location = new int[2];

    private final Paint paint;

    public PosPicker(@NonNull Context context, PickerCallback pickerCallback, List<Pos> poses) {
        super(context, pickerCallback);
        floatCallback = new TouchCallback();
        this.poses = poses;
        FloatFragmentPickerPosBinding binding = FloatFragmentPickerPosBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());
        binding.saveButton.setOnClickListener(v -> {
            pickerCallback.call(this);
            dismiss();
        });

        paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(AppUtil.getAttrColor(getContext(), R.attr.colorSecondary, R.color.amber_500));
        paint.setStrokeWidth(10);
        paint.setStrokeCap(Paint.Cap.ROUND);
    }

    @SuppressLint("DrawAllocation")
    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        getLocationOnScreen(location);

        if (poses != null && currPoints.size() == 0){
            points.clear();
            for (Pos pos : poses) {
                pos = AppUtil.percent2px(getContext(), pos);
                currPoints.add(new Point(pos.getX(), pos.getY()));
            }
            points.addAll(currPoints);
        }
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

    public List<Pos> getPoses() {
        ArrayList<Pos> poses = new ArrayList<>();
        for (Point point : points) {
            poses.add(AppUtil.px2percent(getContext(), new Pos(point.x, point.y)));
        }
        return poses;
    }

    public void onTouch(MotionEvent event){
        float x = event.getRawX();
        float y = event.getRawY();
        switch (event.getAction()){
            case MotionEvent.ACTION_DOWN:
                lastX = x;
                lastY = y;
                isDrag = false;
                currPoints.clear();
                currPoints.add(new Point((int) x, (int) y));
                break;
            case MotionEvent.ACTION_MOVE:
                float dx = x - lastX;
                float dy = y - lastY;
                if (!isDrag && dx * dx + dy * dy < 81) return;
                isDrag = true;
                currPoints.add(new Point((int) x, (int) y));
                lastX = x;
                lastY = y;
                break;
            case MotionEvent.ACTION_UP:
                isDrag = false;
                postDelayed(() -> {
                    points.clear();
                    points.addAll(DouglasPeucker.compress(currPoints));
                }, 50);
        }
        postInvalidate();
    }

    private class TouchCallback extends FloatShowPickerCallback{
        @Override
        public void onTouch(MotionEvent motionEvent) {
            super.onTouch(motionEvent);
            PosPicker.this.onTouch(motionEvent);
        }
    }
}
