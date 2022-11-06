package top.bogey.touch_tool.database.bean.condition;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import top.bogey.touch_tool.database.bean.TaskType;

public abstract class TaskCondition implements Parcelable {
    private final TaskType type;

    public TaskCondition(TaskType type) {
        this.type = type;
    }

    public TaskType getType() {
        return type;
    }

    public static final Creator<TaskCondition> CREATOR = new Creator<TaskCondition>() {
        @Override
        public TaskCondition createFromParcel(Parcel in) {
            TaskType type = TaskType.values()[in.readByte()];
            TaskCondition condition = null;
            switch (type) {
                case IT_IS_TIME:
                    condition = new TimeCondition(in);
                    break;
                case NEW_NOTIFICATION:
                    condition = new NotificationCondition(in);
                    break;
            }

            return condition;
        }

        @Override
        public TaskCondition[] newArray(int size) {
            return new TaskCondition[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeByte((byte) type.ordinal());
    }
}
