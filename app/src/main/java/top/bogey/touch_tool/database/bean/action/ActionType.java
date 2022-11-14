package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import top.bogey.touch_tool.R;

public enum ActionType implements Parcelable {
    NULL,
    NUMBER,
    DELAY,
    TEXT,
    TOUCH,
    IMAGE,
    COLOR,
    SYSTEM,
    TASK,
    INPUT;

    public int getTypeResource(){
        switch (this){
            case DELAY:
                return R.drawable.icon_action_delay;
            case TEXT:
                return R.drawable.icon_action_text;
            case TOUCH:
                return R.drawable.icon_action_touch;
            case IMAGE:
                return R.drawable.icon_action_image;
            case COLOR:
                return R.drawable.icon_action_color;
            case SYSTEM:
                return R.drawable.icon_action_system;
            case TASK:
                return R.drawable.icon_action_task;
            case INPUT:
                return R.drawable.icon_action_input;
        }
        return R.drawable.icon_close;
    }

    public String getTypeName(Context context){
        switch (this) {
            case NULL:
                return context.getString(R.string.action_type_none);
            case NUMBER:
                return context.getString(R.string.action_type_number);
            case DELAY:
                return context.getString(R.string.action_type_delay);
            case TEXT:
                return context.getString(R.string.action_type_text);
            case TOUCH:
                return context.getString(R.string.action_type_touch);
            case IMAGE:
                return context.getString(R.string.action_type_image);
            case COLOR:
                return context.getString(R.string.action_type_color);
            case SYSTEM:
                return context.getString(R.string.action_type_system);
            case TASK:
                return context.getString(R.string.action_type_task);
            case INPUT:
                return context.getString(R.string.action_type_input);
        }
        return null;
    }

    public String getTypeCondition(Context context){
        switch (this) {
            case NULL:
                return context.getString(R.string.action_type_condition_none);
            case NUMBER:
                return context.getString(R.string.action_type_condition_number);
            case TEXT:
                return context.getString(R.string.action_type_condition_text);
            case IMAGE:
                return context.getString(R.string.action_type_condition_image);
        }
        return null;
    }

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
