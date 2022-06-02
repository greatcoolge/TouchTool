package top.bogey.auto_touch;

import android.app.Application;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

import androidx.annotation.NonNull;
import androidx.collection.ArrayMap;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.node.TaskNode;
import top.bogey.auto_touch.room.data.TaskRepository;
import top.bogey.auto_touch.ui.apps.AppInfo;

public class MainViewModel extends AndroidViewModel {
    public final MutableLiveData<Boolean> showSystem = new MutableLiveData<>(false);

    private final TaskRepository repository;
    public final LiveData<List<TaskNode.TaskGroup>> taskGroups;

    private final List<PackageInfo> allApp = new ArrayList<>();

    public MainViewModel(@NonNull Application application) {
        super(application);
        repository = new TaskRepository(application);
        taskGroups = repository.getTaskGroupsLive();
        refreshAppList();
    }

    public void refreshAppList(){
        PackageManager manager = getApplication().getPackageManager();
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
            ApplicationInfo applicationInfo = info.applicationInfo;
            if (system || (applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 1){
                String appName = String.valueOf(applicationInfo.loadLabel(manager));
                String pkgName = info.packageName;
                if (!appName.equalsIgnoreCase(pkgName) && (findString.isEmpty() || pkgName.toLowerCase().contains(findString) || appName.toLowerCase().contains(findString))){
                    apps.add(new AppInfo(appName, pkgName, info));
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
            if (pkgName.equals(info.packageName)){
                return new AppInfo(String.valueOf(info.applicationInfo.loadLabel(manager)), pkgName, info);
            }
        }
        return null;
    }

    public List<String> getAllPkgNames(){
        List<String> names = new ArrayList<>();
        names.add(getApplication().getString(R.string.common_package_name));
        for (PackageInfo info : allApp) {
            names.add(info.packageName);
        }
        return names;
    }

    public LiveData<List<Task>> getTasksLiveByPackageName(String pkgName){
        return repository.getTasksLiveByPackageName(pkgName);
    }

    public List<Task> getAllTasks(){
        return repository.getAllTasks();
    }

    public void saveTask(Task task){
        repository.saveTask(task);
    }

    public void saveTask(List<Task> tasks){
        repository.saveTask(tasks);
    }

    public void deleteTask(Task task){
        repository.deleteTask(task);
    }
}
