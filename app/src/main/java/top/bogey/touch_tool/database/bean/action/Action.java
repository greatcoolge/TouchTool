package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;

public abstract class Action implements Parcelable {
    protected final ActionType type;
    protected final TimeArea timeArea = new TimeArea(100, 100);

    protected Action(ActionType type) {
        this.type = type;
    }

    public static final Creator<Action> CREATOR = new Creator<Action>() {
        @Override
        public Action createFromParcel(Parcel in) {
            ActionType actionType = in.readParcelable(ActionType.class.getClassLoader());
            TimeArea timeArea = in.readParcelable(TimeArea.class.getClassLoader());
            Action action;
            switch (actionType) {
                case NUMBER:
                    action = new NumberAction(in);
                    break;
                case DELAY:
                    action = new DelayAction();
                    break;
                case TEXT:
                    action = new TextAction(in);
                    break;
                case IMAGE:
                    action = new ImageAction(in);
                    break;
                case TOUCH:
                    action = new TouchAction(in);
                    break;
                case SYSTEM:
                    action = new SystemAction(in);
                    break;
                case TASK:
                    action = new TaskAction(in);
                    break;
                case COLOR:
                    action = new ColorAction(in);
                    break;
                default:
                    action = new NullAction();
                    break;
            }
            action.timeArea.setTime(timeArea.getMin(), timeArea.getMax());
            return action;
        }

        @Override
        public Action[] newArray(int size) {
            return new Action[size];
        }
    };

    public ActionType getType() {
        return type;
    }

    public TimeArea getTimeArea() {
        return timeArea;
    }

    protected void sleep(int time) {
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    // 判断动作配置是否有效
    public abstract boolean isValid();

    // 校验动作配置，看是否达成条件
    public abstract boolean checkCondition(MainAccessibilityService service);

    // 执行动作
    public abstract boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo);

    // 获取描述
    public abstract String getDescription(Context context, boolean normal);

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(type.name());
        dest.writeParcelable(timeArea, flags);
    }
}
