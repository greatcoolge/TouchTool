package top.bogey.touch_tool.ui.setting;

import android.content.Context;

import top.bogey.touch_tool.utils.DisplayUtils;

public enum LogLevel {
    LOW,
    MIDDLE,
    HEIGHT;

    public int getLevelColor(Context context){
        int color = 0;
        switch (this) {
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
