package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.graphics.Path;
import android.graphics.Point;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;
import top.bogey.touch_tool.ui.setting.SettingSave;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;

public class TouchAction extends Action {
    private final int screen;
    private final FloatGravity gravity;
    private final Point offset;
    private List<TouchPath> paths = new ArrayList<>();

    private boolean touchOffset = true;

    public TouchAction() {
        super(ActionType.TOUCH);
        screen = 1080;
        gravity = FloatGravity.TOP_LEFT;
        offset = new Point(0, 0);
    }

    public TouchAction(Context context, List<TouchPath> paths) {
        super(ActionType.TOUCH);
        screen = DisplayUtils.getScreen(context);
        gravity = FloatGravity.TOP_LEFT;
        offset = new Point(0, 0);
        this.paths.addAll(paths);
    }

    public TouchAction(Context context, FloatGravity gravity, Point offset, List<TouchPath> paths) {
        super(ActionType.TOUCH);
        screen = DisplayUtils.getScreen(context);
        this.gravity = gravity;
        this.offset = offset;
        this.paths.addAll(paths);
    }

    protected TouchAction(Parcel in) {
        super(ActionType.TOUCH);
        paths = in.createTypedArrayList(TouchPath.CREATOR);
        gravity = FloatGravity.values()[in.readByte()];
        offset = in.readParcelable(Point.class.getClassLoader());
        screen = in.readInt();
        touchOffset = in.readByte() != 0;
    }

    public int getPathLen() {
        for (TouchPath path : paths) {
            if (path.getPoints().size() > 2) return path.getPoints().size();
        }
        return paths.size() > 0 ? 1 : 0;
    }

    public List<TouchPath> getPaths(Context context) {
        Point start = getStartScreenPoint(context);
        int width = DisplayUtils.getScreen(context);
        float scale = width * 1f / screen;

        List<TouchPath> pathList = new ArrayList<>();
        for (TouchPath path : paths) {
            pathList.add(new TouchPath(path.points, start, scale));
        }
        return pathList;
    }

    public List<Path> getPaths(Context context, boolean fixed) {
        List<Path> paths = new ArrayList<>();
        List<TouchPath> touchPaths = getPaths(context);
        for (TouchPath touchPath : touchPaths) {
            paths.add(touchPath.getPath(fixed));
        }
        return paths;
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

    @Override
    public boolean isValid() {
        return getPathLen() > 0;
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        return isValid();
    }

    @Override
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        if (!super.doAction(task, service, runningInfo)) return false;

        List<Path> paths = getPaths(service, true);
        if (paths == null || paths.size() == 0) return false;
        int time = getTimeArea().getRandomTime();
        service.runGesture(paths, time, null);
        sleep(time);
        return true;
    }

    @Override
    public String getDescription(Context context, Task task, Behavior behavior) {
        if (context == null) return null;
        String touch = context.getString(getPathLen() > 1 ? R.string.slide : R.string.touch);
        return context.getString(R.string.action_touch, touch);
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeTypedList(paths);
        dest.writeByte((byte) gravity.ordinal());
        dest.writeParcelable(offset, flags);
        dest.writeInt(screen);
        dest.writeByte((byte) (touchOffset ? 1 : 0));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;

        TouchAction action = (TouchAction) o;

        if (screen != action.screen) return false;
        if (touchOffset != action.touchOffset) return false;
        if (gravity != action.gravity) return false;
        if (!offset.equals(action.offset)) return false;
        return Objects.equals(paths, action.paths);
    }

    @Override
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + screen;
        result = 31 * result + gravity.hashCode();
        result = 31 * result + offset.hashCode();
        result = 31 * result + (paths != null ? paths.hashCode() : 0);
        result = 31 * result + (touchOffset ? 1 : 0);
        return result;
    }

    public static class TouchPath implements Parcelable {
        private transient int pointerId = -1;
        private List<Point> points = new ArrayList<>();

        public TouchPath() {
        }

        public TouchPath(Parcel in) {
            points = in.createTypedArrayList(Point.CREATOR);
        }

        public TouchPath(List<Point> points, Point offset, float scale) {
            for (Point point : points) {
                this.points.add(new Point((int) (point.x * scale) + offset.x, (int) (point.y * scale) + offset.y));
            }
        }

        public void addPoint(int x, int y) {
            if (points.size() != 0) {
                Point point = points.get(points.size() - 1);
                if (point.x == x && point.y == y) return;
            }
            points.add(new Point(x, y));
        }

        public void offset(int x, int y) {
            points.forEach(point -> point.set(point.x + x, point.y + y));
        }

        public void toLine(){
            if (points.size() > 2) {
                setPoints(Arrays.asList(points.get(0), points.get(points.size() - 1)));
            }
        }

        public static final Creator<TouchPath> CREATOR = new Creator<TouchPath>() {
            @Override
            public TouchPath createFromParcel(Parcel in) {
                return new TouchPath(in);
            }

            @Override
            public TouchPath[] newArray(int size) {
                return new TouchPath[size];
            }
        };

        public Path getPath(boolean fixed) {
            Path tmp = null;
            for (Point point : points) {
                if (fixed) point = SettingSave.getInstance().getOffsetPosition(point.x, point.y);
                if (tmp == null) {
                    tmp = new Path();
                    tmp.moveTo(Math.max(point.x, 0), Math.max(point.y, 0));
                } else {
                    tmp.lineTo(Math.max(point.x, 0), Math.max(point.y, 0));
                }
            }
            return tmp;
        }

        @Override
        public int describeContents() {
            return 0;
        }

        @Override
        public void writeToParcel(@NonNull Parcel dest, int flags) {
            dest.writeTypedList(points);
        }

        public int getPointerId() {
            return pointerId;
        }

        public void setPointerId(int pointerId) {
            this.pointerId = pointerId;
        }

        public List<Point> getPoints() {
            return points;
        }

        public void setPoints(List<Point> points) {
            this.points = points;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            TouchPath path = (TouchPath) o;

            return points.equals(path.points);
        }

        @Override
        public int hashCode() {
            return points.hashCode();
        }
    }
}
