package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.os.Parcel;

import androidx.annotation.NonNull;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.BehaviorMode;
import top.bogey.touch_tool.database.bean.Task;

public class NumberAction extends Action {
    private boolean status = true;
    private int targetNum;
    private transient int currNum;

    public NumberAction() {
        super(ActionType.NUMBER);
        targetNum = 1;
    }

    protected NumberAction(Parcel in) {
        super(ActionType.NUMBER);
        status = in.readByte() != 0;
        targetNum = in.readInt();
    }

    @Override
    public boolean isValid() {
        return targetNum > 0;
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        return targetNum <= currNum;
    }

    @Override
    public String getConditionContent(Context context, Task task, Behavior behavior) {
        if (behavior.getBehaviorMode() == BehaviorMode.LOOP) return context.getString(status ? R.string.condition_number_success_for_loop : R.string.condition_number_fail_for_loop, targetNum);
        else return context.getString(status ? R.string.condition_number_success_for_parallel : R.string.condition_number_fail_for_parallel, targetNum);
    }

    @Override
    public String getConditionHint(Context context, Task task, Behavior behavior) {
        if (behavior.getBehaviorMode() == BehaviorMode.LOOP) return context.getString(status ? R.string.condition_number_for_success_loop_tips : R.string.condition_number_for_fail_loop_tips);
        else return context.getString(status ? R.string.condition_number_for_success_parallel_tips : R.string.condition_number_for_fail_parallel_tips);
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeByte((byte) (status ? 1 : 0));
        dest.writeInt(targetNum);
    }

    public boolean isStatus() {
        return status;
    }

    public void setStatus(boolean status) {
        this.status = status;
    }

    public int getTargetNum() {
        return targetNum;
    }

    public void setTargetNum(int targetNum) {
        this.targetNum = targetNum;
    }

    public void addCurrNum(boolean status) {
        if (status == this.status) {
            currNum++;
        }
    }
}
