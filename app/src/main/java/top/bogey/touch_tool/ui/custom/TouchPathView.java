package top.bogey.touch_tool.ui.custom;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.view.View;

import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.room.bean.node.TouchNode;
import top.bogey.touch_tool.utils.DisplayUtils;

public class TouchPathView extends View {
    private final List<Point> points = new ArrayList<>();
    private final List<Point> showPoints = new ArrayList<>();
    private Paint paint;
    private final Point size = new Point();
    private final int lineWidth = 5;
    private boolean isInit = true;

    public TouchPathView(Context context) {
        super(context);
        init();
    }

    public TouchPathView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public TouchPathView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    private void init() {
        paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(DisplayUtils.getAttrColor(getContext(), com.google.android.material.R.attr.colorPrimary, 0));
        paint.setStrokeWidth(lineWidth);
        paint.setStrokeJoin(Paint.Join.ROUND);
        paint.setStrokeCap(Paint.Cap.ROUND);
        paint.setStyle(Paint.Style.STROKE);
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        size.set(getWidth() - lineWidth * 2, getHeight() - lineWidth * 2);
        if (isInit) postInvalidate();
        isInit = false;
    }

    @Override
    public void draw(Canvas canvas) {
        super.draw(canvas);

        formatPoints(points);

        if (showPoints.size() >= 2) {
            Path path = new Path();
            for (Point point : showPoints) {
                if (path.isEmpty()) path.moveTo(point.x, point.y);
                else path.lineTo(point.x, point.y);
            }
            canvas.drawPath(path, paint);
        }

        if (showPoints.size() >= 1) {
            Point point = showPoints.get(showPoints.size() - 1);
            canvas.drawCircle(point.x, point.y, 3, paint);
        }
    }

    public void setPath(TouchNode.TouchPath path) {
        points.clear();
        points.addAll(path.getPoints());
        postInvalidate();
    }

    public void formatPoints(List<Point> points) {
        Rect area = DisplayUtils.calculatePointArea(points);

        float xScale, yScale, xOffset, yOffset;

        if (area.width() == 0) {
            xScale = 1;
        } else {
            xScale = size.x * 1f / area.width();
        }

        if (area.height() == 0) {
            yScale = 1;
        } else {
            yScale = size.y * 1f / area.height();
        }

        float scale = Math.min(xScale, yScale);
        xOffset = (size.x - area.width() * scale) / 2f;
        yOffset = (size.y - area.height() * scale) / 2f;

        showPoints.clear();
        for (Point point : points) {
            int x = point.x - area.left;
            int y = point.y - area.top;
            x = (int) (x * scale + xOffset);
            y = (int) (y * scale + yOffset);
            showPoints.add(new Point(x + lineWidth, y + lineWidth));
        }
    }
}
