package top.bogey.touch_tool;

import android.annotation.SuppressLint;
import android.app.Application;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.MutableLiveData;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.ui.app.AppInfo;
import top.bogey.touch_tool.utils.AppUtils;

public class MainViewModel extends AndroidViewModel {
    public final MutableLiveData<Boolean> showSystem = new MutableLiveData<>(false);

    private final List<PackageInfo> allApp = new ArrayList<>();

    public final MutableLiveData<Task> copyTask = new MutableLiveData<>(null);

    public MainViewModel(@NonNull Application application) {
        super(application);
        refreshAppList();
    }

    @SuppressLint("QueryPermissionsNeeded")
    public void refreshAppList() {
        PackageManager manager = getApplication().getPackageManager();
        allApp.clear();
        allApp.addAll(manager.getInstalledPackages(PackageManager.GET_ACTIVITIES));
    }

    public List<AppInfo> searchAppList(String findString) {
        return searchAppList(findString, true);
    }

    public List<AppInfo> searchAppList(String findString, boolean includeCommon) {
        List<AppInfo> apps = new ArrayList<>();
        if (findString.isEmpty() && includeCommon) {
            apps.add(new AppInfo(getApplication().getString(R.string.common_name), getApplication().getString(R.string.common_package_name), null));
        }
        PackageManager manager = getApplication().getPackageManager();
        findString = findString.toLowerCase();
        boolean system = Boolean.TRUE.equals(showSystem.getValue());
        for (PackageInfo info : allApp) {
            if (!info.packageName.equals(getApplication().getPackageName())) {
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

    public AppInfo getAppInfoByPkgName(String pkgName) {
        if (pkgName.equals(getApplication().getString(R.string.common_package_name))) {
            return new AppInfo(getApplication().getString(R.string.common_name), getApplication().getString(R.string.common_package_name), null);
        }
        PackageManager manager = getApplication().getPackageManager();
        for (PackageInfo info : allApp) {
            if (pkgName.equals(info.packageName) && !pkgName.equals(getApplication().getPackageName())) {
                return new AppInfo(String.valueOf(info.applicationInfo.loadLabel(manager)), pkgName, info);
            }
        }
        return null;
    }

    public List<String> getAllPkgNames() {
        List<String> names = new ArrayList<>();
        names.add(getApplication().getString(R.string.common_package_name));
        for (PackageInfo info : allApp) {
            if (!info.packageName.equals(getApplication().getPackageName()))
                names.add(info.packageName);
        }
        return names;
    }

    public Task getCopyTask() {
        return copyTask.getValue();
    }

    public void setCopyTask(Task task) {
        if (task != null) {
            Task copy = AppUtils.copy(task);
            copy.setId(UUID.randomUUID().toString());
            copyTask.setValue(copy);
        } else copyTask.setValue(null);
    }
}
