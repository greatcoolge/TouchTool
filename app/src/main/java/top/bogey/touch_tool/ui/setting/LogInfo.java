package top.bogey.touch_tool.ui.setting;

import java.text.DateFormat;
import java.util.Date;

public class LogInfo {
    private final String date;
    private final String log;

    private final LogLevel level;

    public LogInfo(String log, LogLevel level) {
        this.log = log;
        this.level = level;
        DateFormat dateFormat = DateFormat.getDateTimeInstance();
        Date date = new Date(System.currentTimeMillis());
        this.date = dateFormat.format(date);
    }

    public String getDate() {
        return date;
    }

    public String getLog() {
        return log;
    }

    public LogLevel getLevel() {
        return level;
    }
}
