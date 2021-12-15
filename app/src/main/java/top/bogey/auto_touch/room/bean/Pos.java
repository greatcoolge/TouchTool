package top.bogey.auto_touch.room.bean;

import androidx.annotation.NonNull;

public class Pos {
    private int x;
    private int y;

    public Pos(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public void setX(int x) {
        this.x = x;
    }

    public int getY() {
        return y;
    }

    public void setY(int y) {
        this.y = y;
    }

    @NonNull
    @Override
    public String toString() {
        return "(" + x + "," + y + ")";
    }
}
