package top.bogey.touch_tool.ui.setting;

import android.content.Context;
import android.util.Log;

import com.tencent.mmkv.MMKV;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;
import top.bogey.touch_tool.ui.running.RunningInfo;

public class LogUtils {
    private static final List<LogListener> listenerList = new ArrayList<>();

    public static final String RUNNING_LOG = "running_log";
    private static final MMKV logMMKV = MMKV.mmkvWithID(RUNNING_LOG, MMKV.SINGLE_PROCESS_MODE, RUNNING_LOG);
    public static final String RUNNING_TASKS = "running_tasks";
    private static final MMKV taskMMKV = MMKV.mmkvWithID(RUNNING_TASKS, MMKV.SINGLE_PROCESS_MODE, RUNNING_TASKS);

    private static long date = 0L;

    public static void init() {
        date = System.currentTimeMillis();
    }

    public static String getRunningTime() {
        DateFormat timeInstance = SimpleDateFormat.getTimeInstance();
        return timeInstance.format(new Date(System.currentTimeMillis() - date));
    }

    public static void run(Context context, Task task, String pkgName, boolean success) {
        log(LogLevel.MIDDLE, String.format("%s - %s\n%s", task.getTitle(), pkgName, context.getString(success ? R.string.log_run_task_result_success : R.string.log_run_task_result_fail, task.getTitle())));
        if (!SettingSave.getInstance().isRunningTaskInfo()) return;
        RunningInfo runningInfo = new RunningInfo(task.getId(), pkgName, success);
        taskMMKV.encode(runningInfo.getId(), runningInfo);
    }

    public static Map<String, Map<String, List<RunningInfo>>> getRunningInfo() {
        Map<String, Map<String, List<RunningInfo>>> map = new HashMap<>();
        String[] keys = taskMMKV.allKeys();
        if (keys != null) {
            for (String key : keys) {
                RunningInfo runningInfo = taskMMKV.decodeParcelable(key, RunningInfo.class);
                if (runningInfo != null) {
                    Map<String, List<RunningInfo>> pkgMap = map.get(runningInfo.getPkgName());
                    if (pkgMap == null) pkgMap = new HashMap<>();
                    List<RunningInfo> taskList = pkgMap.get(runningInfo.getTaskId());
                    if (taskList == null) taskList = new ArrayList<>();
                    taskList.add(runningInfo);
                    taskList.sort((o1, o2) -> (int) (o1.getDate() - o2.getDate()));
                    pkgMap.put(runningInfo.getTaskId(), taskList);
                    map.put(runningInfo.getPkgName(), pkgMap);
                }
            }
        }
        return map;
    }

    public static void log(LogLevel level, String log) {
        boolean enabledLog = SettingSave.getInstance().isRunningLog();
        LogInfo logInfo = new LogInfo(log, level);
        Log.d(RUNNING_LOG, logInfo.getLog());
        if (enabledLog) {
            logMMKV.encode(logInfo.getId(), logInfo);
            for (int i = listenerList.size() - 1; i >= 0; i--) {
                LogListener logListener = listenerList.get(i);
                if (logListener != null) logListener.newInfo(logInfo);
                else listenerList.remove(i);
            }
        }
    }

    public static void log(LogLevel level, Context context, boolean result, Task task, TaskRunningInfo info, String content) {
        String format = String.format("%s - %s\n[%s]%s", task.getTitle(), info.getPkgName(), info.getProgress(), content);
        log(level, context.getString(result ? R.string.log_run_task_result_success : R.string.log_run_task_result_fail, format));
    }

    public static List<LogInfo> getLogs(LogListener listener) {
        if (listener != null && !listenerList.contains(listener)) listenerList.add(listener);
        List<LogInfo> logs = new ArrayList<>();

        String[] keys = logMMKV.allKeys();
        if (keys != null) {
            for (String key : keys) {
                logs.add(logMMKV.decodeParcelable(key, LogInfo.class));
            }
        }
        logs.sort((o1, o2) -> (int) (o1.getDate() - o2.getDate()));
        return logs;
    }

    public static void closeLog() {
        logMMKV.clearAll();
    }

    public static void cleanRunningInfo() {
        taskMMKV.clearAll();
    }

    public interface LogListener {
        void newInfo(LogInfo info);
    }
}
