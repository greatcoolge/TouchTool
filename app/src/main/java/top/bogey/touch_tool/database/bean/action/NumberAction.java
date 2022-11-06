package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.os.Parcel;

import androidx.annotation.NonNull;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;

public class NumberAction extends Action {
    private boolean status;
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
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        runningInfo.addProgress(task, this, false);
        return false;
    }

    @Override
    public String getDescription(Context context, boolean normal) {
        return context.getString(R.string.action_image);
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
        if (status == this.status){
            currNum++;
        }
    }
}
