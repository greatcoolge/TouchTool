package top.bogey.auto_touch.ui;

import android.app.Application;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.data.TaskGroup;
import top.bogey.auto_touch.room.data.TaskRepository;
import top.bogey.auto_touch.ui.apps.AppInfo;
import top.bogey.auto_touch.util.AppUtil;

public class MainViewModel extends AndroidViewModel {
    private static final String SAVE_PATH = "save";
    private static final String SAVE_KEY = "ServiceEnable";
    public final MutableLiveData<Boolean> serviceEnable = new MutableLiveData<>(false);
    public final MutableLiveData<Boolean> showSystem = new MutableLiveData<>(false);
    public LiveData<List<TaskGroup>> taskGroups;
    private final List<ApplicationInfo> allApps = new ArrayList<>();
    private final TaskRepository repository;


    public MainViewModel(Application application){
        super(application);
        loadServiceEnable();
        repository = new TaskRepository(application);
        taskGroups = repository.getTaskGroupsLive();
        refreshAppList();
    }

    private void loadServiceEnable(){
        SharedPreferences sharedPreferences = getApplication().getSharedPreferences(SAVE_PATH, Context.MODE_PRIVATE);
        String string = sharedPreferences.getString(SAVE_KEY, "false");
        boolean result = false;
        if (string != null){
            result = string.equals("true");
        }
        serviceEnable.setValue(result && AppUtil.isAccessibilityServiceOn(getApplication()));
    }

    public void saveServiceEnable(boolean enable){
        SharedPreferences sharedPreferences = getApplication().getSharedPreferences(SAVE_PATH, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPreferences.edit();
        editor.putString(SAVE_KEY, String.valueOf(enable));
        editor.apply();
        loadServiceEnable();
    }

    public boolean isServiceEnable(){
        return serviceEnable.getValue() != null && serviceEnable.getValue();
    }

    public void refreshAppList(){
        allApps.clear();
        PackageManager manager = getApplication().getPackageManager();
        List<PackageInfo> infoList = manager.getInstalledPackages(PackageManager.GET_ACTIVITIES);
        for (PackageInfo info : infoList) {
            allApps.add(info.applicationInfo);
        }
    }

    public List<AppInfo> searchAppList(String findString){
        List<AppInfo> apps = new ArrayList<>();
        if (findString.isEmpty()){
            apps.add(new AppInfo(getApplication().getString(R.string.common_name), getApplication().getString(R.string.common_package_name), null));
        }
        PackageManager manager = getApplication().getPackageManager();
        findString = findString.toLowerCase();
        boolean showSystem = isShowSystem();
        for (ApplicationInfo info : allApps) {
            if (showSystem || (info.flags & ApplicationInfo.FLAG_SYSTEM) != 1){
                String appName = String.valueOf(info.loadLabel(manager));
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
        for (ApplicationInfo info : allApps) {
            if (pkgName.equals(info.packageName)){
                return new AppInfo(String.valueOf(info.loadLabel(manager)), pkgName, info);
            }
        }
        return null;
    }

    public boolean isShowSystem() {
        Boolean value = showSystem.getValue();
        return value != null && value;
    }

    public List<String> getAllPkgNames(){
        List<String> names = new ArrayList<>();
        names.add(getApplication().getString(R.string.common_package_name));
        for (ApplicationInfo info : allApps) {
            names.add(info.packageName);
        }
        return names;
    }

    public LiveData<List<Task>> getTasksLiveByPackageName(String pkgName){
        return repository.getTasksLiveByPackageName(pkgName);
    }

    public List<Task> getTasksByPackageName(String pkgName){
        return repository.getTasksByPackageName(pkgName);
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
