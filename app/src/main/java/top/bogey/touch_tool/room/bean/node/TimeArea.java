package top.bogey.touch_tool.room.bean.node;

public class TimeArea{
    private int min;
    private int max;

    public TimeArea(int time) {
        this(time, time);
    }

    public TimeArea(int min, int max) {
        this.min = min;
        this.max = max;
    }

    public void setTime(int min, int max){
        this.min = min;
        this.max = max;
    }

    public void setTime(int time){
        setTime(time, time);
    }

    public int getRealMax(){
        return Math.max(min, max);
    }

    public int getRealMin(){
        return Math.min(min, max);
    }

    public int getRandomTime(){
        return (int) Math.round(Math.random() * Math.abs(max - min) + getRealMin());
    }

    public int getMin() {
        return min;
    }

    public void setMin(int min) {
        this.min = min;
    }

    public int getMax() {
        return max;
    }

    public void setMax(int max) {
        this.max = max;
    }

    public String getTitle(){
        if (min == max) return String.valueOf(min);
        return getRealMin() + "-" + getRealMax();
    }
}
