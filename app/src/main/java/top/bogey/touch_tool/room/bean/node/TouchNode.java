package top.bogey.touch_tool.room.bean.node;

import android.content.Context;
import android.graphics.Path;
import android.graphics.Point;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;

public class TouchNode extends Node {
    public TouchNode(TouchPath touchPath) {
        super(NodeType.TOUCH, touchPath);
    }

    public void setValue(Context context, List<Point> points) {
        setValue(new TouchPath(context, points));
    }

    @Override
    public TouchPath getValue() {
        return (TouchPath) value;
    }

    @Override
    public boolean isValid() {
        TouchPath touchPath = getValue();
        return touchPath != null && touchPath.getPathLen() > 0;
    }

    @Override
    public boolean checkNode(Object obj) {
        return isValid();
    }

    @Override
    public Object getNodeTarget(Object obj) {
        Context context = (Context) obj;
        return getValue().getPath(context, true);
    }

    @Override
    public Object cloneValue() {
        return getValue();
    }

    public static class TouchPath {
        private final List<Point> path;
        private final FloatGravity gravity;

        private final Point offset;
        private final int screen;

        private boolean touchOffset = true;

        public TouchPath(Context context) {
            path = new ArrayList<>();
            gravity = FloatGravity.TOP_LEFT;
            offset = new Point(0, 0);
            screen = DisplayUtils.getScreen(context);
        }

        public TouchPath(Context context, List<Point> points) {
            path = points;
            gravity = FloatGravity.TOP_LEFT;
            offset = new Point(0, 0);
            screen = DisplayUtils.getScreen(context);
        }

        public TouchPath(List<Point> path, FloatGravity gravity, Point offset, int screen, boolean touchOffset) {
            this.path = path;
            this.gravity = gravity;
            this.offset = offset;
            this.screen = screen;
            this.touchOffset = touchOffset;
        }

        public int getPathLen() {
            return path.size();
        }

        public Path getPath(Context context, boolean fixed) {
            List<Point> points = getPoints(context);
            Path tmp = null;
            for (Point point : points) {
                point = getPoint(point, fixed);
                if (tmp == null) {
                    tmp = new Path();
                    tmp.moveTo(point.x, point.y);
                } else {
                    tmp.lineTo(point.x, point.y);
                }
            }
            return tmp;
        }

        public List<Point> getPoints() {
            return path;
        }

        private Point getPoint(Point point, boolean fixed) {
            if (fixed) return AppUtils.getFixedPosition(point.x, point.y);
            return point;
        }

        public List<Point> getPoints(Context context) {
            Point start = getStartScreenPoint(context);
            int width = DisplayUtils.getScreen(context);
            float scale = width * 1f / screen;
            List<Point> points = new ArrayList<>();
            for (Point point : path) {
                points.add(new Point((int) (point.x * scale) + start.x, (int) (point.y * scale) + start.y));
            }
            return points;
        }

        private Point getStartScreenPoint(Context context) {
            int width = DisplayUtils.getScreen(context);
            float scale = width * 1f / screen;
            Point start = new Point((int) (offset.x * scale), (int) (offset.y * scale));
            Point size = DisplayUtils.getScreenSize(context);
            switch (gravity) {
                case TOP_LEFT:
                    break;
                case TOP_RIGHT:
                    start.x = start.x + size.x;
                    break;
                case BOTTOM_LEFT:
                    start.y = start.y + size.y;
                    break;
                case BOTTOM_RIGHT:
                    start.x = start.x + size.x;
                    start.y = start.y + size.y;
                    break;
            }
            return start;
        }

        public FloatGravity getGravity() {
            return gravity;
        }

        public boolean isTouchOffset() {
            return touchOffset;
        }

        public void setTouchOffset(boolean touchOffset) {
            this.touchOffset = touchOffset;
        }
    }
}
