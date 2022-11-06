package top.bogey.touch_tool.database.bean.action;

import android.content.Context;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;

public class NullAction extends Action {
    public NullAction() {
        super(ActionType.NULL);
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        return true;
    }

    @Override
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        runningInfo.addProgress(task, this, false);
        return false;
    }

    @Override
    public String getDescription(Context context, boolean normal) {
        return null;
    }
}
