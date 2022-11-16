package top.bogey.touch_tool.ui.task;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public enum SortType implements Parcelable {
    // 创建时间正/反
    CREATE_TIME_ASC,
    CREATE_TIME_DESC,
    // 修改时间正/反
    MODIFY_TIME_ASC,
    MODIFY_TIME_DESC;

    public static final Creator<SortType> CREATOR = new Creator<SortType>() {
        @Override
        public SortType createFromParcel(Parcel in) {
            return SortType.values()[in.readByte()];
        }

        @Override
        public SortType[] newArray(int size) {
            return new SortType[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeByte((byte) ordinal());
    }
}
