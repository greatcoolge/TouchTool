package top.bogey.touch_tool.database.bean.condition;

import android.os.Parcel;

import androidx.annotation.NonNull;

import java.util.Objects;

import top.bogey.touch_tool.database.bean.TaskType;

public class NotificationCondition extends TaskCondition{
    // 通知匹配的文字
    private String text;

    public NotificationCondition() {
        super(TaskType.NEW_NOTIFICATION);
    }

    public NotificationCondition(Parcel in) {
        super(TaskType.NEW_NOTIFICATION);
        text = in.readString();
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeString(text);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NotificationCondition that = (NotificationCondition) o;
        return Objects.equals(text, that.text);
    }

    @Override
    public int hashCode() {
        return Objects.hash(text);
    }
}
