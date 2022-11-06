package top.bogey.touch_tool.ui.tasks;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public enum ShotType implements Parcelable {
    // 创建时间正/反
    CREATE_TIME_ASC,
    CREATE_TIME_DESC,
    // 修改时间正/反
    MODIFY_TIME_ASC,
    MODIFY_TIME_DESC;

    public static final Creator<ShotType> CREATOR = new Creator<ShotType>() {
        @Override
        public ShotType createFromParcel(Parcel in) {
            return ShotType.values()[in.readByte()];
        }

        @Override
        public ShotType[] newArray(int size) {
            return new ShotType[size];
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
