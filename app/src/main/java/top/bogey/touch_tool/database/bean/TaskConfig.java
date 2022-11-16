package top.bogey.touch_tool.database.bean;

import android.os.Parcel;
import android.os.Parcelable;
import android.util.Log;

import androidx.annotation.NonNull;

public class TaskConfig implements Parcelable {
    private final String id;
    private final long createTime;
    private long modifyTime;
    private String tag;

    public TaskConfig(String id) {
        this.id = id;
        createTime = System.currentTimeMillis();
        modifyTime = createTime;
    }

    protected TaskConfig(Parcel in) {
        id = in.readString();
        createTime = in.readLong();
        modifyTime = in.readLong();
        tag = in.readString();
    }

    public static final Creator<TaskConfig> CREATOR = new Creator<TaskConfig>() {
        @Override
        public TaskConfig createFromParcel(Parcel in) {
            return new TaskConfig(in);
        }

        @Override
        public TaskConfig[] newArray(int size) {
            return new TaskConfig[size];
        }
    };

    public String getId() {
        return id;
    }

    public long getCreateTime() {
        return createTime;
    }

    public long getModifyTime() {
        return modifyTime;
    }

    public void setModifyTime(long modifyTime) {
        this.modifyTime = modifyTime;
    }

    public String getTag() {
        return tag;
    }

    public void setTag(String tag) {
        this.tag = tag;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(id);
        dest.writeLong(createTime);
        dest.writeLong(modifyTime);
        dest.writeString(tag);
    }
}
