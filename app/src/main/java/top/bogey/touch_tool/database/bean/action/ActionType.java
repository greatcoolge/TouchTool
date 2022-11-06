package top.bogey.touch_tool.database.bean.action;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public enum ActionType implements Parcelable {
    NULL,
    NUMBER,
    DELAY,
    TEXT,
    TOUCH,
    IMAGE,
    COLOR,
    SYSTEM,
    TASK;

    public static final Creator<ActionType> CREATOR = new Creator<ActionType>() {
        @Override
        public ActionType createFromParcel(Parcel in) {
            return ActionType.values()[in.readByte()];
        }

        @Override
        public ActionType[] newArray(int size) {
            return new ActionType[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeByte((byte) ordinal());
    }
}
