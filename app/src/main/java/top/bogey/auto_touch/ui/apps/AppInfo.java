package top.bogey.auto_touch.ui.apps;

import android.content.pm.ApplicationInfo;

public class AppInfo {
    public String appName;
    public String packageName;
    public ApplicationInfo info;

    public AppInfo(String appName, String packageName, ApplicationInfo info) {
        this.appName = appName;
        this.packageName = packageName;
        this.info = info;
    }
}
