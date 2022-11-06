package top.bogey.touch_tool;

import android.accessibilityservice.AccessibilityService;
import android.accessibilityservice.GestureDescription;
import android.app.Activity;
import android.app.Notification;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.graphics.Path;
import android.os.Build;
import android.os.IBinder;
import android.util.Log;
import android.view.accessibility.AccessibilityEvent;

import androidx.lifecycle.MutableLiveData;
import androidx.work.Data;
import androidx.work.ExistingPeriodicWorkPolicy;
import androidx.work.ExistingWorkPolicy;
import androidx.work.OneTimeWorkRequest;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;

import com.tencent.mmkv.MMKV;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskType;
import top.bogey.touch_tool.database.bean.condition.NotificationCondition;
import top.bogey.touch_tool.database.bean.condition.TimeCondition;
import top.bogey.touch_tool.database.data.FindRunnable;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.database.data.TaskRunnable;
import top.bogey.touch_tool.database.data.TaskWorker;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.RunningUtils;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.ResultCallback;
import top.bogey.touch_tool.utils.TaskRunningCallback;

public class MainAccessibilityService extends AccessibilityService {
    private static final String SERVICE_ENABLED = "service_enabled";

    // 服务
    private boolean serviceConnected = false;
    public static final MutableLiveData<Boolean> serviceEnabled = new MutableLiveData<>(false);

    // 截屏
    public static final MutableLiveData<Boolean> captureEnabled = new MutableLiveData<>(false);
    public MainCaptureService.CaptureServiceBinder binder = null;
    private ServiceConnection connection = null;

    // 任务
    private final ExecutorService findService;
    public final ExecutorService taskService;
    private FindRunnable findRunnable = null;
    public String currPkgName = "";
    // 运行中的任务
    private final List<TaskRunnable> taskRunnableList = new ArrayList<>();
    // 外部任务监视器
    private final List<TaskRunningCallback> taskOverSee = new ArrayList<>();


    public MainAccessibilityService() {
        findService = new ThreadPoolExecutor(2, 20, 60L, TimeUnit.SECONDS, new ArrayBlockingQueue<>(20));
        taskService = new ThreadPoolExecutor(3, 20, 60L, TimeUnit.SECONDS, new ArrayBlockingQueue<>(20));
    }

    @Override
    public void onAccessibilityEvent(AccessibilityEvent event) {
        Log.d("TAG", "onAccessibilityEvent: " + event);
        if (event != null && isServiceEnabled()) {
            if (event.getEventType() == AccessibilityEvent.TYPE_WINDOWS_CHANGED) {
                boolean flag;
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                    flag = (event.getWindowChanges() & AccessibilityEvent.WINDOWS_CHANGE_ADDED) != 0;
                } else {
                    flag = true;
                }
                if (flag) {
                    if (findRunnable != null) findRunnable.stop();
                    findRunnable = new FindRunnable(this, currPkgName);
                    findService.execute(findRunnable);
                }

            } else if (event.getEventType() == AccessibilityEvent.TYPE_NOTIFICATION_STATE_CHANGED) {
                // 需要是正常的通知
                if (!Notification.class.getName().contentEquals(event.getClassName())) return;

                // 且通知里带了文本
                List<CharSequence> eventText = event.getText();
                if (eventText == null || eventText.size() == 0) return;

                // 获取所有可执行的任务
                List<Task> comTasks = TaskRepository.getInstance().getTasksByPkgName(getString(R.string.common_package_name));
                String packageName = String.valueOf(event.getPackageName());
                if (!"null".equals(packageName)) {
                    List<Task> pkgTasks = TaskRepository.getInstance().getTasksByPkgName(packageName);
                    comTasks.addAll(pkgTasks);
                }

                for (Task task : comTasks) {
                    if (task.getType() == TaskType.NEW_NOTIFICATION) {
                        NotificationCondition condition = (NotificationCondition) task.getCondition();
                        Pattern pattern = Pattern.compile(condition.getText());
                        for (CharSequence charSequence : eventText) {
                            if (pattern.matcher(charSequence).find()) {
                                runTask(task, null);
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    @Override
    public void onInterrupt() {
    }

    @Override
    protected void onServiceConnected() {
        super.onServiceConnected();
        serviceConnected = true;
        MainApplication.setService(this);

        setServiceEnabled(MMKV.defaultMMKV().decodeBool(SERVICE_ENABLED, false));
    }

    @Override
    public boolean onUnbind(Intent intent) {
        serviceConnected = false;
        MainApplication.setService(null);
        stopCaptureService();

        WorkManager.getInstance(this).cancelAllWork();

        return super.onUnbind(intent);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        MainApplication.setService(this);
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        serviceConnected = false;
        MainApplication.setService(null);
        stopCaptureService();

        WorkManager.getInstance(this).cancelAllWork();
    }

    public boolean isServiceConnected() {
        return serviceConnected;
    }

    public void setServiceEnabled(boolean enabled) {
        serviceEnabled.setValue(enabled);
        MMKV.defaultMMKV().encode(SERVICE_ENABLED, enabled);

        if (isServiceEnabled()) {
            List<Task> tasks = TaskRepository.getInstance().getTasksByType(TaskType.IT_IS_TIME);
            if (tasks != null) {
                for (Task task : tasks) {
                    addWork(task);
                }
            }
        } else {
            WorkManager.getInstance(this).cancelAllWork();
        }
    }

    public boolean isServiceEnabled() {
        return isServiceConnected() && Boolean.TRUE.equals(serviceEnabled.getValue());
    }

    public void startCaptureService(boolean moveBack, ResultCallback callback) {
        if (binder == null) {
            MainActivity activity = MainApplication.getActivity();
            if (activity != null) {
                activity.launchCapture(((code, data) -> {
                    if (code == Activity.RESULT_OK) {
                        connection = new ServiceConnection() {
                            @Override
                            public void onServiceConnected(ComponentName name, IBinder service) {
                                binder = (MainCaptureService.CaptureServiceBinder) service;
                                captureEnabled.setValue(true);
                                if (moveBack) activity.moveTaskToBack(true);
                                if (callback != null) callback.onResult(true);
                            }

                            @Override
                            public void onServiceDisconnected(ComponentName name) {
                                stopCaptureService();
                            }
                        };
                        Intent intent = new Intent(this, MainCaptureService.class);
                        intent.putExtra("Data", data);
                        boolean result = bindService(intent, connection, Context.BIND_AUTO_CREATE);
                        if (!result) if (callback != null) callback.onResult(false);
                    } else {
                        if (callback != null) callback.onResult(false);
                    }
                }));
            } else {
                if (callback != null) callback.onResult(false);
            }
        } else {
            if (callback != null) callback.onResult(true);
        }
    }

    public void stopCaptureService() {
        if (connection != null) {
            unbindService(connection);
            stopService(new Intent(this, MainCaptureService.class));
            connection = null;
        }
        binder = null;
        captureEnabled.setValue(false);
    }

    public boolean isCaptureEnabled() {
        return isServiceConnected() && Boolean.TRUE.equals(captureEnabled.getValue());
    }

    public List<TaskRunnable> getRunningTasks(TaskRunningCallback callback) {
        if (!taskOverSee.contains(callback)) taskOverSee.add(callback);
        return taskRunnableList;
    }

    public void removeRunStateCallback(TaskRunningCallback callback) {
        taskOverSee.remove(callback);
    }

    public TaskRunnable runTask(Task task, TaskRunningCallback callback) {
        if (task == null || !isServiceEnabled()) return null;
        TaskRunnable runnable = new TaskRunnable(task, this, currPkgName);
        runnable.addCallback(callback);
        runnable.addCallback(new TaskRunningCallback() {
            @Override
            public void onStart(TaskRunnable runnable) {
                taskRunnableList.add(runnable);
            }

            @Override
            public void onProgress(TaskRunnable runnable, int percent) {
            }

            @Override
            public void onEnd(TaskRunnable runnable, boolean succeed) {
                synchronized (taskRunnableList) {
                    taskRunnableList.remove(runnable);
                }
            }
        });
        runnable.addCallbacks(taskOverSee);
        taskService.submit(runnable);
        return runnable;
    }

    public void stopTask(TaskRunnable runnable) {
        stopTask(runnable, false);
    }

    public void stopTask(TaskRunnable runnable, boolean force) {
        runnable.stop(force);
    }

    public void stopAllTask(boolean force) {
        for (TaskRunnable runnable : taskRunnableList) {
            stopTask(runnable, force);
        }
    }

    public void addWork(Task task) {
        if (task == null || !isServiceEnabled()) return;

        if (task.getType() == TaskType.IT_IS_TIME) {
            WorkManager workManager = WorkManager.getInstance(this);
            long timeMillis = System.currentTimeMillis();
            TimeCondition timeCondition = (TimeCondition) task.getCondition();
            if (timeCondition.getPeriodic() > 0) {
                // 尽量小延迟的执行间隔任务
                PeriodicWorkRequest workRequest = new PeriodicWorkRequest.Builder(TaskWorker.class, timeCondition.getPeriodic(), TimeUnit.MINUTES, 5, TimeUnit.MINUTES)
                        .setInitialDelay(timeCondition.getStartTime() - timeMillis, TimeUnit.MILLISECONDS)
                        .setInputData(new Data.Builder()
                                .putString("id", task.getId())
                                .putString("title", task.getTitle())
                                .build())
                        .build();
                workManager.enqueueUniquePeriodicWork(task.getId(), ExistingPeriodicWorkPolicy.REPLACE, workRequest);
                RunningUtils.log(LogLevel.MIDDLE, getString(R.string.log_add_periodic_job, task.getTitle(), AppUtils.formatDateMinute(timeCondition.getStartTime()), timeCondition.getPeriodic() / 60, timeCondition.getPeriodic() % 60));
            } else if (timeCondition.getStartTime() > timeMillis) {
                OneTimeWorkRequest workRequest = new OneTimeWorkRequest.Builder(TaskWorker.class)
                        .setInitialDelay(timeCondition.getStartTime() - timeMillis, TimeUnit.MILLISECONDS)
                        .setInputData(new Data.Builder()
                                .putString("id", task.getId())
                                .putString("title", task.getTitle())
                                .build())
                        .build();
                workManager.enqueueUniqueWork(task.getId(), ExistingWorkPolicy.REPLACE, workRequest);
                RunningUtils.log(LogLevel.MIDDLE, getString(R.string.log_add_job, task.getTitle(), AppUtils.formatDateMinute(timeCondition.getStartTime())));
            } else {
                // 所有条件都未达到，任务无效，尝试移除已有的任务
                removeWork(task);
            }
        }
    }

    public void removeWork(Task task) {
        WorkManager workManager = WorkManager.getInstance(this);
        workManager.cancelUniqueWork(task.getId());
        RunningUtils.log(LogLevel.MIDDLE, getString(R.string.log_remove_job, task.getTitle()));
    }

    public void runGesture(Path path, int time, ResultCallback callback) {
        dispatchGesture(new GestureDescription.Builder().addStroke(new GestureDescription.StrokeDescription(path, 0, time)).build(), new GestureResultCallback() {
            @Override
            public void onCompleted(GestureDescription gestureDescription) {
                super.onCompleted(gestureDescription);
                if (callback != null) callback.onResult(true);
            }

            @Override
            public void onCancelled(GestureDescription gestureDescription) {
                super.onCancelled(gestureDescription);
                if (callback != null) callback.onResult(false);
            }
        }, null);
    }
}
