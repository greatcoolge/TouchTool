package top.bogey.touch_tool.ui.running;

import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.UUID;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.utils.AppUtils;

public class RunningInfo implements Parcelable {
    private final String id;
    private final String taskId;
    private final String pkgName;
    private final boolean success;
    private final long date;

    public RunningInfo(String taskId, String pkgName, boolean success) {
        this.id = UUID.randomUUID().toString();
        this.taskId = taskId;
        this.pkgName = pkgName;
        this.success = success;
        this.date = System.currentTimeMillis();
    }

    protected RunningInfo(Parcel in) {
        id = in.readString();
        taskId = in.readString();
        pkgName = in.readString();
        success = in.readInt() == 1;
        date = in.readLong();
    }

    public String getId() {
        return id;
    }

    public String getTaskId() {
        return taskId;
    }

    public String getPkgName() {
        return pkgName;
    }

    public boolean isSuccess() {
        return success;
    }

    public long getDate() {
        return date;
    }

    public String getDateString(Context context) {
        return context.getString(R.string.date, AppUtils.formatDateLocalDate(context, date), AppUtils.formatDateLocalSecond(context, date));
    }

    public static final Creator<RunningInfo> CREATOR = new Creator<RunningInfo>() {
        @Override
        public RunningInfo createFromParcel(Parcel in) {
            return new RunningInfo(in);
        }

        @Override
        public RunningInfo[] newArray(int size) {
            return new RunningInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(id);
        dest.writeString(taskId);
        dest.writeString(pkgName);
        dest.writeInt(success ? 1 : 0);
        dest.writeLong(date);
    }
}
