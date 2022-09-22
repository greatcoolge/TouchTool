package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;

import java.text.DateFormat;
import java.util.Date;

import top.bogey.touch_tool.utils.DisplayUtils;

public class DebugInfo {
    private final String title;
    private final int percent;
    private final String content;
    private final String date;
    private final DebugLevel level;

    public DebugInfo(DebugLevel level, String title, int percent, String content) {
        this.level = level;
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

    public int getLevelColor(Context context){
        int color = 0;
        switch (level) {
            case LOW:
                color = DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorOutline, 0);
                break;
            case MIDDLE:
                color = DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorOnSurface, 0);
                break;
            case HEIGHT:
                color = DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorError, 0);
                break;
        }
        return color;
    }
}
