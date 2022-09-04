package top.bogey.touch_tool.utils;

import android.graphics.Rect;

public class MatchResult {
    public int value;
    public Rect rect;

    public MatchResult(double value, int x, int y, int width, int height) {
        this.value = (int) Math.round(value * 100);
        this.rect = new Rect(x, y, x + width, y + height);
    }
}
