package top.bogey.touch_tool.database.bean.action;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public class TimeArea implements Parcelable {
    private int min;
    private int max;

    public TimeArea(int time) {
        this(time, time);
    }

    public TimeArea(int min, int max) {
        this.min = min;
        this.max = max;
    }

    protected TimeArea(Parcel in) {
        min = in.readInt();
        max = in.readInt();
    }

    public void setTime(int min, int max) {
        this.min = min;
        this.max = max;
    }

    public void setTime(int time) {
        setTime(time, time);
    }

    public int getRandomTime() {
        return (int) Math.round(Math.random() * Math.abs(max - min) + getMin());
    }

    public String getTitle() {
        if (min == max) return String.valueOf(min);
        return getMin() + "-" + getMax();
    }

    public static final Creator<TimeArea> CREATOR = new Creator<TimeArea>() {
        @Override
        public TimeArea createFromParcel(Parcel in) {
            return new TimeArea(in);
        }

        @Override
        public TimeArea[] newArray(int size) {
            return new TimeArea[size];
        }
    };

    public int getMin() {
        return Math.min(min, max);
    }

    public void setMin(int min) {
        this.min = min;
    }

    public int getMax() {
        return Math.max(min, max);
    }

    public void setMax(int max) {
        this.max = max;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(min);
        dest.writeInt(max);
    }
}
