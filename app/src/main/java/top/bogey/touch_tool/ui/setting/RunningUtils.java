package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

import com.tencent.mmkv.MMKV;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.room.bean.Task;

public class RunningUtils {
    private static final List<LogListener> listenerList = new ArrayList<>();

    public static final String RUNNING_LOG = "running_log";
    private static final MMKV logMMKV = MMKV.mmkvWithID(RUNNING_LOG, MMKV.SINGLE_PROCESS_MODE, RUNNING_LOG);
    public static final String RUNNING_TASKS = "running_tasks";
    private static final MMKV taskMMKV = MMKV.mmkvWithID(RUNNING_TASKS, MMKV.SINGLE_PROCESS_MODE, RUNNING_TASKS);

    private static long date = 0L;

    public static void init() {
        date = System.currentTimeMillis();
    }

    public static String getRunningTime(){
        DateFormat timeInstance = SimpleDateFormat.getTimeInstance();
        return timeInstance.format(new Date(System.currentTimeMillis() - date));
    }

    @SuppressLint("DefaultLocale")
    public static void run(Context context, Task task, String pkgName, boolean success){
        RunningInfo runningInfo = new RunningInfo(task.getId(), pkgName, success);
        taskMMKV.encode(runningInfo.getId(), runningInfo);
        log(context, LogLevel.MIDDLE,  String.format("%s-%s\n%s", task.getTitle(), task.getPkgName(), context.getString(R.string.log_run_task_result, context.getString(success ? R.string.log_run_task_result_success : R.string.log_run_task_result_fail))));
    }

    public static Map<String, Map<String, List<RunningInfo>>> getRunningInfo(){
        Map<String, Map<String, List<RunningInfo>>> map = new HashMap<>();
        String[] keys = taskMMKV.allKeys();
        if (keys != null){
            for (String key : keys) {
                RunningInfo runningInfo = taskMMKV.decodeParcelable(key, RunningInfo.class);
                if (runningInfo != null){
                    Map<String, List<RunningInfo>> pkgMap = map.get(runningInfo.getPkgName());
                    if (pkgMap == null) pkgMap = new HashMap<>();
                    List<RunningInfo> taskList = pkgMap.get(runningInfo.getTaskId());
                    if (taskList == null) taskList = new ArrayList<>();
                    taskList.add(runningInfo);
                    taskList.sort((o1, o2) -> o1.getDate() < o2.getDate() ? 1 : 0);
                    pkgMap.put(runningInfo.getTaskId(), taskList);
                    map.put(runningInfo.getPkgName(), pkgMap);
                }
            }
        }
        return map;
    }

    public static void log(Context context, LogLevel level, String log){
        SharedPreferences preferences = context.getSharedPreferences(MainAccessibilityService.SAVE_PATH, Context.MODE_PRIVATE);
        boolean enabledLog = preferences.getBoolean(RUNNING_TASKS, false);
        if (enabledLog){
            LogInfo logInfo = new LogInfo(log, level);
            Log.d(RUNNING_TASKS, logInfo.getLog());
            logMMKV.encode(logInfo.getId(), logInfo);
            for (int i = listenerList.size() - 1; i >= 0 ; i--) {
                LogListener logListener = listenerList.get(i);
                if (logListener != null) logListener.newInfo(logInfo);
                else listenerList.remove(i);
            }
        }
    }

    @SuppressLint("DefaultLocale")
    public static void log(Context context, Task task, int percent, String content, LogLevel level){
        log(context, level, String.format("%s-%s\n[%d]%s", task.getTitle(), task.getPkgName(), percent, content));
    }

    public static List<LogInfo> getLogs(LogListener listener){
        if (listener != null && !listenerList.contains(listener)) listenerList.add(listener);
        List<LogInfo> logs = new ArrayList<>();

        String[] keys = logMMKV.allKeys();
        if (keys != null){
            for (String key : keys) {
                logs.add(logMMKV.decodeParcelable(key, LogInfo.class));
            }
        }
        return logs;
    }

    public static void closeLog(){
        logMMKV.clearAll();
    }

    public interface LogListener {
        void newInfo(LogInfo info);
    }
}
