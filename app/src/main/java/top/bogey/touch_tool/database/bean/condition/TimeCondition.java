package top.bogey.touch_tool.database.bean.condition;

import android.os.Parcel;

import androidx.annotation.NonNull;

import java.util.Objects;

import top.bogey.touch_tool.database.bean.TaskType;

public class TimeCondition extends TaskCondition{
    // 开始时间/秒
    private long startTime;
    // 间隔时间/分钟
    private int periodic;

    public TimeCondition() {
        super(TaskType.IT_IS_TIME);
        startTime = System.currentTimeMillis();
    }

    public TimeCondition(Parcel in) {
        super(TaskType.IT_IS_TIME);
        startTime = in.readLong();
        periodic = in.readInt();
    }

    public long getStartTime() {
        return startTime;
    }

    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    public int getPeriodic() {
        return periodic;
    }

    public void setPeriodic(int periodic) {
        this.periodic = periodic;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeLong(startTime);
        dest.writeInt(periodic);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TimeCondition that = (TimeCondition) o;
        return startTime == that.startTime && periodic == that.periodic;
    }

    @Override
    public int hashCode() {
        return Objects.hash(startTime, periodic);
    }
}
