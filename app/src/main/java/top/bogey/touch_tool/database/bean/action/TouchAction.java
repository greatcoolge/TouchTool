package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.graphics.Path;
import android.graphics.Point;
import android.os.Parcel;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;

public class TouchAction extends Action {
    private final int screen;
    private final FloatGravity gravity;
    private final Point offset;
    private final List<Point> path;

    private boolean touchOffset = true;

    public TouchAction() {
        super(ActionType.TOUCH);
        screen = 1080;
        gravity = FloatGravity.TOP_LEFT;
        offset = new Point(0, 0);
        path = new ArrayList<>();
    }

    public TouchAction(Context context, List<Point> points){
        super(ActionType.TOUCH);
        screen = DisplayUtils.getScreen(context);
        gravity = FloatGravity.TOP_LEFT;
        offset = new Point(0, 0);
        path = points;
    }

    public TouchAction(Context context, FloatGravity gravity, Point offset, List<Point> points){
        super(ActionType.TOUCH);
        screen = DisplayUtils.getScreen(context);
        this.gravity = gravity;
        this.offset = offset;
        path = points;
    }

    protected TouchAction(Parcel in) {
        super(ActionType.TOUCH);
        path = in.createTypedArrayList(Point.CREATOR);
        gravity = FloatGravity.values()[in.readByte()];
        offset = in.readParcelable(Point.class.getClassLoader());
        screen = in.readInt();
        touchOffset = in.readByte() != 0;
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
        Path path = getPath(service, true);
        if (path == null) return false;
        int time = getTimeArea().getRandomTime();
        service.runGesture(path, time, null);
        sleep(time);
        return true;
    }

    @Override
    public String getDescription(Context context, boolean normal) {
        return context.getString(R.string.action_touch);
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeTypedList(path);
        dest.writeByte((byte) gravity.ordinal());
        dest.writeParcelable(offset, flags);
        dest.writeInt(screen);
        dest.writeByte((byte) (touchOffset ? 1 : 0));
    }
}
