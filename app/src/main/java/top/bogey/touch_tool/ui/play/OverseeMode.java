package top.bogey.touch_tool.ui.play;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public enum OverseeMode implements Parcelable {
    CLOSED,
    ACROSS_APP,
    NOT_MANUAL;

    public static final Creator<OverseeMode> CREATOR = new Creator<OverseeMode>() {
        @Override
        public OverseeMode createFromParcel(Parcel in) {
            return OverseeMode.values()[in.readByte()];
        }

        @Override
        public OverseeMode[] newArray(int size) {
            return new OverseeMode[size];
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
