package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;

import java.text.DateFormat;
import java.util.Date;

public class DebugInfo {
    private final String title;
    private final int percent;
    private final String content;
    private final String date;

    public DebugInfo(String title, int percent, String content) {
        this.title = title;
        this.percent = percent;
        this.content = content;
        DateFormat dateFormat = DateFormat.getDateTimeInstance();
        Date date = new Date(System.currentTimeMillis());
        this.date = dateFormat.format(date);
    }

    @SuppressLint("DefaultLocale")
    public String getSimpleInfo(){
        if (percent > 0){
            return String.format("%s [%d] %s", title, percent, content);
        }
        return String.format("%s %s", title, content);
    }

    public String getDebugInfo(){
        return String.format("[%s] %s", date, getSimpleInfo());
    }
}
