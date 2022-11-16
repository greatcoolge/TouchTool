package top.bogey.touch_tool.database.bean;

import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import top.bogey.touch_tool.R;

public enum TaskType implements Parcelable {
    CLOSED,             // 禁用
    MANUAL,             // 手动
    IT_IS_TIME,         // 到时间了
    NEW_NOTIFICATION,   // 收到新通知
    APP_CHANGED,        // 应用变更时
    VIEW_CHANGED,       // 窗口变更
    CONTENT_CHANGED;    // 内容变更

    public int getTypeResource() {
        switch (this) {
            case CLOSED:
                return R.drawable.icon_task_close;
            case MANUAL:
                return R.drawable.icon_task_manual;
            case IT_IS_TIME:
                return R.drawable.icon_task_it_is_time;
            case NEW_NOTIFICATION:
                return R.drawable.icon_task_new_notification;
            case APP_CHANGED:
                return R.drawable.icon_task_app_changed;
            case VIEW_CHANGED:
                return R.drawable.icon_task_view_changed;
            case CONTENT_CHANGED:
                return R.drawable.icon_task_content_changed;
        }
        return 0;
    }

    public String getTypeDescription(Context context) {
        switch (this) {
            case CLOSED:
                return context.getString(R.string.task_type_closed);
            case MANUAL:
                return context.getString(R.string.task_type_manual);
            case IT_IS_TIME:
                return context.getString(R.string.task_type_it_is_time);
            case NEW_NOTIFICATION:
                return context.getString(R.string.task_type_new_notification);
            case APP_CHANGED:
                return context.getString(R.string.task_type_app_changed);
            case VIEW_CHANGED:
                return context.getString(R.string.task_type_view_changed);
            case CONTENT_CHANGED:
                return context.getString(R.string.task_type_content_changed);
        }
        return "";
    }

    public static final Creator<TaskType> CREATOR = new Creator<TaskType>() {
        @Override
        public TaskType createFromParcel(Parcel in) {
            return TaskType.values()[in.readByte()];
        }

        @Override
        public TaskType[] newArray(int size) {
            return new TaskType[size];
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
