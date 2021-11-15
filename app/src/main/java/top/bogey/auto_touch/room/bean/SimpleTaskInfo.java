package top.bogey.auto_touch.room.bean;

import androidx.annotation.NonNull;

public class SimpleTaskInfo {
    public int id;
    public String title;

    public SimpleTaskInfo(int id, String title) {
        this.id = id;
        this.title = title;
    }

    @NonNull
    @Override
    public String toString() {
        return title;
    }
}
