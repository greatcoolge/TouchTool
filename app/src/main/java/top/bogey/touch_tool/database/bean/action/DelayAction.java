package top.bogey.touch_tool.database.bean.action;

import android.content.Context;

import androidx.annotation.Nullable;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;

public class DelayAction extends Action {
    public DelayAction() {
        super(ActionType.DELAY);
        timeArea.setTime(1000);
    }

    public DelayAction(int time) {
        super(ActionType.DELAY);
        timeArea.setTime(time);
    }

    @Override
    public boolean isValid() {
        return getTimeArea().getMin() > 0;
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        return false;
    }

    @Override
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        if (!super.doAction(task, service, runningInfo)) return false;

        sleep(getTimeArea().getRandomTime());
        return true;
    }

    @Override
    public String getDescription(Context context, Task task, Behavior behavior) {
        if (context == null) return timeArea.getTitle();
        return context.getString(R.string.action_delay, timeArea.getTitle());
    }
}
