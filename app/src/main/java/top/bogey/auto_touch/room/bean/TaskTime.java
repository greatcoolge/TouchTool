package top.bogey.auto_touch.room.bean;

import android.content.Context;

public class TaskTime {
    private long time;
    private long interval = 0;

    public TaskTime(long time) {
        this.time = time;
    }

    public long getTime() {
        return time;
    }

    public void setTime(long time) {
        this.time = time;
    }

    public long getInterval() {
        return interval;
    }

    public void setInterval(long interval) {
        this.interval = interval;
    }

    public String getIntervalTitle(Context context){
        return "";
    }
}
