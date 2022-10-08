package top.bogey.touch_tool.ui.setting;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.text.DateFormat;
import java.util.Date;
import java.util.UUID;

public class LogInfo implements Parcelable {
    private final String id;
    private final long date;
    private final String log;

    private final LogLevel level;

    public LogInfo(String log, LogLevel level) {
        id = UUID.randomUUID().toString();
        this.log = log;
        this.level = level;
        this.date = System.currentTimeMillis();
    }

    protected LogInfo(Parcel in) {
        id = in.readString();
        date = in.readLong();
        log = in.readString();
        level = LogLevel.valueOf(in.readString());
    }

    public static final Creator<LogInfo> CREATOR = new Creator<LogInfo>() {
        @Override
        public LogInfo createFromParcel(Parcel in) {
            return new LogInfo(in);
        }

        @Override
        public LogInfo[] newArray(int size) {
            return new LogInfo[size];
        }
    };

    public long getDate() {
        return date;
    }

    public String getDateString(){
        DateFormat dateFormat = DateFormat.getDateTimeInstance();
        Date date = new Date(this.date);
        return dateFormat.format(date);
    }

    public String getId() {
        return id;
    }

    public String getLog() {
        return log;
    }

    public LogLevel getLevel() {
        return level;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(id);
        dest.writeLong(date);
        dest.writeString(log);
        dest.writeString(level.name());
    }
}
