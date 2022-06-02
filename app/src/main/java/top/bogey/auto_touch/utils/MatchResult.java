package top.bogey.auto_touch.utils;

public class MatchResult {
    public int value;
    public int x;
    public int y;

    public MatchResult(double value, int x, int y) {
        this.value = (int) Math.round(value * 100);
        this.x = x;
        this.y = y;
    }
}
