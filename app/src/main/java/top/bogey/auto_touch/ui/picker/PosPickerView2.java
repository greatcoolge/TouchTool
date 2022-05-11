package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentPickerBgBinding;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.DouglasPeucker;

public class PosPickerView2 extends FrameLayout implements NodePickerInterface {
    private final List<Point> points = new ArrayList<>();
    private final List<Point> currPoints = new ArrayList<>();
    private float lastX = 0;
    private float lastY = 0;
    private boolean isDrag = false;

    public PosPickerView2(@NonNull Context context, PickerCallback pickerCallback) {
        super(context);
        FloatFragmentPickerBgBinding binding = FloatFragmentPickerBgBinding.inflate(LayoutInflater.from(context));
        addView(binding.getRoot());
        binding.closeButton.setOnClickListener(v -> pickerCallback.call(this));
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {
        Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(AppUtil.getAttrColor(getContext(), R.attr.colorSecondary, R.color.amber_500));

        float[] pts = new float[currPoints.size() * 2];
        for (int i = 0; i < currPoints.size(); i++) {
            pts[i * 2] = currPoints.get(i).x;
            pts[i * 2 + 1] = currPoints.get(i).y;
        }
        canvas.drawLines(pts, paint);
//        super.draw(canvas);
        super.dispatchDraw(canvas);
    }

    @Override
    public void draw(Canvas canvas) {
        Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(AppUtil.getAttrColor(getContext(), R.attr.colorSecondary, R.color.amber_500));

        float[] pts = new float[currPoints.size() * 2];
        for (int i = 0; i < currPoints.size(); i++) {
            pts[i * 2] = currPoints.get(i).x;
            pts[i * 2 + 1] = currPoints.get(i).y;
        }
        canvas.drawLines(pts, paint);
        super.draw(canvas);
    }

    public List<Point> getPoints() {
        return points;
    }

    public void onTouch(MotionEvent event){
        float x = event.getX();
        float y = event.getY();
        switch (event.getAction()){
            case MotionEvent.ACTION_DOWN:
                lastX = x;
                lastY = y;
                isDrag = false;
                currPoints.clear();
                points.clear();
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
                points.addAll(DouglasPeucker.compress(currPoints));
        }
        postInvalidate();
    }

    @Override
    public void show(int x, int y) {

    }

    @Override
    public void dismiss() {

    }
}
