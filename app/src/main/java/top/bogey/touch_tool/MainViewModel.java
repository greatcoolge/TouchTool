package top.bogey.touch_tool;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.ui.app.AppInfo;
import top.bogey.touch_tool.utils.AppUtils;

public class MainViewModel {
    private static MainViewModel viewModel;

    public final MutableLiveData<Boolean> showSystem = new MutableLiveData<>(false);

    private final Map<CharSequence, PackageInfo> appMap = new HashMap<>();

    public final MutableLiveData<Task> copyTask = new MutableLiveData<>(null);
    public final MutableLiveData<Behavior> copyBehavior = new MutableLiveData<>(null);

    public static MainViewModel getInstance() {
        if (viewModel == null) viewModel = new MainViewModel();
        return viewModel;
    }

    @SuppressLint("QueryPermissionsNeeded")
    public void refreshAppList(Context context) {
        PackageManager manager = context.getPackageManager();
        appMap.clear();
        List<PackageInfo> packages = manager.getInstalledPackages(PackageManager.GET_ACTIVITIES);
        for (PackageInfo packageInfo : packages) {
            appMap.put(packageInfo.packageName, packageInfo);
        }
    }

    public boolean isActivityClass(CharSequence packageName, CharSequence className) {
        if (packageName == null) return false;
        PackageInfo packageInfo = appMap.get(packageName);
        if (packageInfo == null) return false;
        for (ActivityInfo activityInfo : packageInfo.activities) {
            if (TextUtils.equals(className, activityInfo.name)) return true;
        }
        return false;
    }

    public List<AppInfo> searchAppList(Context context, String findString) {
        return searchAppList(context, findString, true);
    }

    public List<AppInfo> searchAppList(Context context, String findString, boolean includeCommon) {
        List<AppInfo> apps = new ArrayList<>();
        if (findString.isEmpty() && includeCommon) {
            apps.add(new AppInfo(context.getString(R.string.common_name), context.getString(R.string.common_package_name), null));
        }
        PackageManager manager = context.getPackageManager();
        findString = findString.toLowerCase();
        boolean system = Boolean.TRUE.equals(showSystem.getValue());

        for (Map.Entry<CharSequence, PackageInfo> entry : appMap.entrySet()) {
            PackageInfo info = entry.getValue();
            if (!info.packageName.equals(context.getPackageName())) {
                ApplicationInfo applicationInfo = info.applicationInfo;
                if (system || (applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 1) {
                    String appName = String.valueOf(applicationInfo.loadLabel(manager));
                    String pkgName = info.packageName;
                    if (!appName.equalsIgnoreCase(pkgName) && (findString.isEmpty() || pkgName.toLowerCase().contains(findString) || appName.toLowerCase().contains(findString))) {
                        apps.add(new AppInfo(appName, pkgName, info));
                    }
                }
            }
        }
        return apps;
    }

    public AppInfo getAppInfoByPkgName(Context context, String pkgName) {
        if (pkgName.equals(context.getString(R.string.common_package_name))) {
            return new AppInfo(context.getString(R.string.common_name), context.getString(R.string.common_package_name), null);
        }
        PackageManager manager = context.getPackageManager();
        for (Map.Entry<CharSequence, PackageInfo> entry : appMap.entrySet()) {
            PackageInfo info = entry.getValue();
            if (pkgName.equals(info.packageName) && !pkgName.equals(context.getPackageName())) {
                return new AppInfo(String.valueOf(info.applicationInfo.loadLabel(manager)), pkgName, info);
            }
        }
        return null;
    }

    public List<String> getAllPkgNames(Context context) {
        List<String> names = new ArrayList<>();
        names.add(context.getString(R.string.common_package_name));
        for (Map.Entry<CharSequence, PackageInfo> entry : appMap.entrySet()) {
            PackageInfo info = entry.getValue();
            if (!info.packageName.equals(context.getPackageName()))
                names.add(info.packageName);
        }
        return names;
    }

    public Task getCopyTask() {
        Task task = copyTask.getValue();
        if (task == null) return null;
        Task copy = AppUtils.copy(task);
        copy.setId(UUID.randomUUID().toString());
        return copy;
    }

    public void setCopyTask(Task task) {
        copyTask.setValue(task);
    }

    public Behavior getCopyBehavior() {
        Behavior behavior = copyBehavior.getValue();
        if (behavior == null) return null;
        return AppUtils.copy(behavior);
    }

    public void setCopyBehavior(Behavior behavior) {
        copyBehavior.setValue(behavior);
    }
}
