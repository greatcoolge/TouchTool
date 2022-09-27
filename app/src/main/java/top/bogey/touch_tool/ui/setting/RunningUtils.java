package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

import com.google.gson.Gson;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.room.bean.Task;

public class RunningUtils {
    public static final String RUNNING_LOG = "running_log";
    private static final String LOG_PRE_NAME = "log_";
    private static final List<LogListener> listenerList = new ArrayList<>();
    private static final Gson gson = new Gson();

    private static final Map<String, RunningInfo> runningInfoMap = new HashMap<>();
    private static long date = 0L;

    public static void init() {
        date = System.currentTimeMillis();
        runningInfoMap.clear();
    }

    public static String getRunningTime(){
        DateFormat timeInstance = SimpleDateFormat.getTimeInstance();
        return timeInstance.format(new Date(System.currentTimeMillis() - date));
    }

    public static void run(Task task, boolean success){
        RunningInfo runningInfo = runningInfoMap.getOrDefault(task.getId(), new RunningInfo(task.getId()));
        if (runningInfo != null){
            runningInfo.addCount(success);
        }
    }

    public static Map<String, RunningInfo> getRunningInfoMap(){
        return runningInfoMap;
    }

    public static String getLogFileName(Context context){
        File dir = context.getCacheDir();
        DateFormat format = SimpleDateFormat.getDateInstance();
        return dir.getAbsolutePath() + File.separator + LOG_PRE_NAME + format.format(new Date(System.currentTimeMillis()));
    }

    public static void log(Context context, LogLevel level, String log){
        SharedPreferences preferences = context.getSharedPreferences(MainAccessibilityService.SAVE_PATH, Context.MODE_PRIVATE);
        boolean enabledLog = preferences.getBoolean(RUNNING_LOG, false);
        LogInfo logInfo = new LogInfo(log, level);
        Log.d(RUNNING_LOG, logInfo.getLog());
        if (enabledLog){
            File file = new File(getLogFileName(context));
            try (FileOutputStream outputStream = new FileOutputStream(file, true)){
                outputStream.write((gson.toJson(logInfo) + '\n').getBytes());
            } catch (IOException e) {
                e.printStackTrace();
            }
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

    public static List<LogInfo> getLogs(Context context, LogListener listener){
        if (listener != null && !listenerList.contains(listener)) listenerList.add(listener);
        List<LogInfo> logs = new ArrayList<>();

        File file = new File(getLogFileName(context));
        try(FileInputStream inputStream = new FileInputStream(file)){
            InputStreamReader reader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
            BufferedReader bufferedReader = new BufferedReader(reader);
            while (bufferedReader.ready()){
                LogInfo logInfo = gson.fromJson(bufferedReader.readLine(), LogInfo.class);
                if (logInfo != null) logs.add(logInfo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return logs;
    }

    public static void closeLog(Context context){
        File dir = context.getCacheDir();
        if (dir.exists() && dir.isDirectory()){
            File[] files = dir.listFiles();
            if (files != null){
                for (File listFile : files) {
                    if (listFile.getName().contains(LOG_PRE_NAME)) listFile.delete();
                }
            }
        }
    }

    public interface LogListener {
        void newInfo(LogInfo info);
    }
}
