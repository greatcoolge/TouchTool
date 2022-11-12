package top.bogey.touch_tool.database.bean.action;

import android.content.Context;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.BehaviorMode;
import top.bogey.touch_tool.database.bean.Task;

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
    public String getConditionContent(Context context, Task task, Behavior behavior) {
        if (behavior.getBehaviorMode() == BehaviorMode.CONDITION) return context.getString(R.string.condition_null_for_condition);
        else return context.getString(R.string.condition_null_for_loop);
    }

    @Override
    public String getConditionHint(Context context, Task task, Behavior behavior) {
        if (behavior.getBehaviorMode() == BehaviorMode.CONDITION) return context.getString(R.string.condition_null_for_condition_tips);
        else return context.getString(R.string.condition_null_for_loop_tips);
    }
}
