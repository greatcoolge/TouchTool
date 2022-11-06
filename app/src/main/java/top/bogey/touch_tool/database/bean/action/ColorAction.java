package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.Rect;
import android.os.Parcel;

import androidx.annotation.NonNull;

import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;

public class ColorAction extends Action {
    private int screen;
    private int[] color = new int[]{-1, -1, -1};
    private int minPercent = 0;
    private int maxPercent = 100;
    private int size = 0;

    public ColorAction() {
        super(ActionType.COLOR);
    }

    public ColorAction(Context context, int[] color, int minPercent, int maxPercent, int size) {
        super(ActionType.COLOR);
        screen = DisplayUtils.getScreen(context);
        this.color = color;
        this.minPercent = minPercent;
        this.maxPercent = maxPercent;
        this.size = size;
    }

    protected ColorAction(Parcel in) {
        super(ActionType.COLOR);
        screen = in.readInt();
        color = in.createIntArray();
        minPercent = in.readInt();
        maxPercent = in.readInt();
        size = in.readInt();
    }

    @Override
    public boolean isValid() {
        for (int i : color) {
            if (i == -1) return false;
        }
        return true;
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        return false;
    }

    @Override
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        if (!service.isCaptureEnabled()) return false;

        List<Rect> rectList = service.binder.matchColor(color);
        if (rectList == null || rectList.isEmpty()) return false;

        for (int i = rectList.size() - 1; i >= 0; i--) {
            Rect rect = rectList.get(i);
            int size = rect.width() * rect.height();
            if (size < getMinSize(service) || size > getMaxsize(service)) {
                rectList.remove(i);
            }
        }
        if (rectList.isEmpty()) return false;

        for (Rect rect : rectList) {
            Path path = new Path();
            Point fixedPosition = AppUtils.getFixedPosition(rect.centerX(), rect.centerY());
            path.moveTo(fixedPosition.x, fixedPosition.y);
            service.runGesture(path, 100, null);
            sleep(getTimeArea().getRandomTime());
        }
        runningInfo.addProgress(task, this, false);
        return true;
    }

    @Override
    public String getDescription(Context context, boolean normal) {
        return context.getString(R.string.action_color);
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeInt(screen);
        dest.writeIntArray(color);
        dest.writeInt(minPercent);
        dest.writeInt(maxPercent);
        dest.writeInt(size);
    }

    public int getScreen() {
        return screen;
    }

    public void setScreen(int screen) {
        this.screen = screen;
    }

    public int[] getColor() {
        return color;
    }

    public void setColor(int[] color) {
        this.color = color;
    }

    public int getMinPercent() {
        return minPercent;
    }

    public void setMinPercent(int minPercent) {
        this.minPercent = minPercent;
    }

    public int getMaxPercent() {
        return maxPercent;
    }

    public void setMaxPercent(int maxPercent) {
        this.maxPercent = maxPercent;
    }

    public int getSize() {
        return size;
    }

    public int getSize(Context context) {
        int screen = DisplayUtils.getScreen(context);
        return (int) (size * 1f * screen / this.screen);
    }

    public void setSize(int size) {
        this.size = size;
    }

    public int getMinSize(Context context) {
        return (int) (getSize(context) * minPercent / 100f);
    }

    public int getMaxsize(Context context) {
        return (int) (getSize(context) * maxPercent / 100f);
    }
}
