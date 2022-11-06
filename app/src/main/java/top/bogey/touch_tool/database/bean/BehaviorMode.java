package top.bogey.touch_tool.database.bean;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import top.bogey.touch_tool.R;

public enum BehaviorMode implements Parcelable {
    CONDITION,
    LOOP,
    PARALLEL;

    public int getTypeResource() {
        switch (this) {
            case CONDITION:
                return R.drawable.icon_behavior_condition;
            case LOOP:
                return R.drawable.icon_behavior_loop;
            case PARALLEL:
                return R.drawable.icon_behavior_parallel;
        }
        return 0;
    }

    public static final Creator<BehaviorMode> CREATOR = new Creator<BehaviorMode>() {
        @Override
        public BehaviorMode createFromParcel(Parcel in) {
            return BehaviorMode.values()[in.readByte()];
        }

        @Override
        public BehaviorMode[] newArray(int size) {
            return new BehaviorMode[size];
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
