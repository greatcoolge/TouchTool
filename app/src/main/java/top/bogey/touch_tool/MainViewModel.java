package top.bogey.touch_tool;

import android.annotation.SuppressLint;
import android.app.Application;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.node.TaskNode;
import top.bogey.touch_tool.room.data.TaskRepository;
import top.bogey.touch_tool.ui.apps.AppInfo;

public class MainViewModel extends AndroidViewModel {
    public final MutableLiveData<Boolean> showSystem = new MutableLiveData<>(false);

    private final TaskRepository repository;
    public final LiveData<List<TaskNode.TaskGroup>> taskGroups;

    private final List<PackageInfo> allApp = new ArrayList<>();

    public final MutableLiveData<Task> copyTask = new MutableLiveData<>(null);

    public MainViewModel(@NonNull Application application) {
        super(application);
        repository = new TaskRepository(application);
        taskGroups = repository.getTaskGroupsLive();
        refreshAppList();
    }

    @SuppressLint("QueryPermissionsNeeded")
    public void refreshAppList(){
        PackageManager manager = getApplication().getPackageManager();
        allApp.clear();
        allApp.addAll(manager.getInstalledPackages(PackageManager.GET_ACTIVITIES));
    }

    public List<AppInfo> searchAppList(String findString){
        List<AppInfo> apps = new ArrayList<>();
        if (findString.isEmpty()){
            apps.add(new AppInfo(getApplication().getString(R.string.common_name), getApplication().getString(R.string.common_package_name), null));
        }
        PackageManager manager = getApplication().getPackageManager();
        findString = findString.toLowerCase();
        boolean system = Boolean.TRUE.equals(showSystem.getValue());
        for (PackageInfo info : allApp) {
            if (!info.packageName.equals(getApplication().getPackageName())){
                ApplicationInfo applicationInfo = info.applicationInfo;
                if (system || (applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 1){
                    String appName = String.valueOf(applicationInfo.loadLabel(manager));
                    String pkgName = info.packageName;
                    if (!appName.equalsIgnoreCase(pkgName) && (findString.isEmpty() || pkgName.toLowerCase().contains(findString) || appName.toLowerCase().contains(findString))){
                        apps.add(new AppInfo(appName, pkgName, info));
                    }
                }
            }
        }
        return apps;
    }

    public AppInfo getAppInfoByPkgName(String pkgName){
        if (pkgName.equals(getApplication().getString(R.string.common_package_name))){
            return new AppInfo(getApplication().getString(R.string.common_name), getApplication().getString(R.string.common_package_name), null);
        }
        PackageManager manager = getApplication().getPackageManager();
        for (PackageInfo info : allApp) {
            if (pkgName.equals(info.packageName) && !pkgName.equals(getApplication().getPackageName())){
                return new AppInfo(String.valueOf(info.applicationInfo.loadLabel(manager)), pkgName, info);
            }
        }
        return null;
    }

    public List<String> getAllPkgNames(){
        List<String> names = new ArrayList<>();
        names.add(getApplication().getString(R.string.common_package_name));
        for (PackageInfo info : allApp) {
            if (!info.packageName.equals(getApplication().getPackageName()))
                names.add(info.packageName);
        }
        return names;
    }

    public LiveData<List<Task>> getTasksLiveByPackageName(String pkgName){
        return repository.getTasksLiveByPackageName(pkgName);
    }

    public Task getCopyTask() {
        return copyTask.getValue();
    }

    public void setCopyTask(Task task) {
        if (task != null) copyTask.setValue(new Task(task));
        else copyTask.setValue(null);
    }
}
