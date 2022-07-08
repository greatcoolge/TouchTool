package top.bogey.auto_touch.room.data;

import android.content.Intent;
import android.util.Log;
import android.view.accessibility.AccessibilityNodeInfo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.room.bean.Task;

public class FindRunnable implements Runnable{
    private final MainAccessibilityService service;
    private final List<TaskRunnable> tasks;
    private final String pkgName;

    public FindRunnable(MainAccessibilityService service, List<TaskRunnable> tasks, String pkgName) {
        this.service = service;
        this.tasks = tasks;
        this.pkgName = pkgName;
    }

    @Override
    public void run() {
        AccessibilityNodeInfo root = service.getRootInActiveWindow();
        int times = 0;
        while (root == null){
            times++;
            if (times > 20) break;
            try{Thread.sleep(50);} catch (InterruptedException ignored){}
            root = service.getRootInActiveWindow();
        }
        if (root != null){
            String packageName = String.valueOf(root.getPackageName());
            if (!(packageName.equals("null") || packageName.equals(pkgName))){
                // 取消所有非当前包下的任务
                for (TaskRunnable task : tasks) {
                    if (task.isRunning()) task.stop();
                }
                tasks.clear();

                service.setCurrPkgName(packageName);

                MainActivity activity = MainApplication.getActivity();
                if (activity != null){
                    activity.dismissPlayFloatView();
                }

                // APP自己不执行任何任务
                if (packageName.equals(service.getPackageName())) return;

                Log.d("EnterApp", packageName);

                List<Task> tasks = getAppTaskByPkgName(packageName);

                String comPkgName = service.getString(R.string.common_package_name);
                boolean isManual = false;
                for (Task task : tasks) {
                    if (task.getActions() != null && !task.getActions().isEmpty()){
                        switch (task.getStatus()) {
                            case AUTO:
                                service.runTask(task, null);
                                break;
                            case MANUAL:
                                String name = task.getPkgName();
                                if (!name.equals(comPkgName)){
                                    isManual = true;
                                }
                                break;
                        }
                    }
                }
                if (isManual){
                    if (activity != null){
                        activity.showPlayFloatView(packageName);
                    } else {
                        Intent intent = new Intent(service, MainActivity.class);
                        intent.putExtra("IsBackground", true);
                        intent.putExtra("FloatPackageName", packageName);
                        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                        service.startActivity(intent);
                    }
                }
            }
        }
    }

    private List<Task> getAppTaskByPkgName(String pkgName){
        TaskRepository repository = new TaskRepository(service);
        Map<String, Task> taskMap = new HashMap<>();

        String conPkgName = service.getString(R.string.common_package_name);
        List<Task> comTasks = repository.getTasksByPackageName(conPkgName);
        for (Task task : comTasks) {
            taskMap.put(task.getTitle(), task);
        }

        List<Task> pkgTasks = repository.getTasksByPackageName(pkgName);
        for (Task task : pkgTasks) {
            taskMap.put(task.getTitle(), task);
        }
        return (List<Task>) taskMap.values();
    }
}
