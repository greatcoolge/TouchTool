package top.bogey.auto_touch.ui.apps;


import android.content.pm.PackageInfo;
import android.os.Parcel;
import android.os.Parcelable;

public class AppInfo implements Parcelable {
    public String appName;
    public String packageName;
    public PackageInfo info;

    public AppInfo(String appName, String packageName, PackageInfo info) {
        this.appName = appName;
        this.packageName = packageName;
        this.info = info;
    }

    protected AppInfo(Parcel in) {
        appName = in.readString();
        packageName = in.readString();
        info = in.readParcelable(PackageInfo.class.getClassLoader());
    }

    public static final Creator<AppInfo> CREATOR = new Creator<AppInfo>() {
        @Override
        public AppInfo createFromParcel(Parcel in) {
            return new AppInfo(in);
        }

        @Override
        public AppInfo[] newArray(int size) {
            return new AppInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(appName);
        dest.writeString(packageName);
        dest.writeParcelable(info, flags);
    }
}
