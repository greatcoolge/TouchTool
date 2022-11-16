package top.bogey.touch_tool.ui.setting;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public enum NightMode implements Parcelable {
    AUTO,
    OPENED,
    CLOSED;

    public int getModeValue() {
        switch (this) {
            case AUTO:
                return -1;
            case OPENED:
                return 2;
            case CLOSED:
                return 1;
        }
        return -1;
    }

    public static final Creator<NightMode> CREATOR = new Creator<NightMode>() {
        @Override
        public NightMode createFromParcel(Parcel in) {
            return NightMode.values()[in.readByte()];
        }

        @Override
        public NightMode[] newArray(int size) {
            return new NightMode[size];
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
