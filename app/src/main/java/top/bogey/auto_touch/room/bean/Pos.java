package top.bogey.auto_touch.room.bean;

import androidx.annotation.NonNull;

public class Pos {
    public int x;
    public int y;

    public Pos(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @NonNull
    @Override
    public String toString() {
        return "(" + x + "," + y + ")";
    }
}
