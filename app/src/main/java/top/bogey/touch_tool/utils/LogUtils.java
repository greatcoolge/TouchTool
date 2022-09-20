package top.bogey.touch_tool.utils;

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
import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.ui.setting.DebugInfo;

public class LogUtils {
    public static final String ACTION_DEBUG = "action_debug";
    private static final String LOG_PRE_NAME = "log_";
    private static final List<LogListener> listenerList = new ArrayList<>();
    private static final Gson gson = new Gson();

    public static String getLogFileName(Context context){
        File dir = context.getCacheDir();
        DateFormat format = SimpleDateFormat.getDateInstance();
        Date date = new Date(System.currentTimeMillis());
        return dir.getAbsolutePath() + File.separator + LOG_PRE_NAME + format.format(date);
    }

    public static synchronized void log(Context context, String title, int percent, String content){
        SharedPreferences preferences = context.getSharedPreferences(MainAccessibilityService.SAVE_PATH, Context.MODE_PRIVATE);
        boolean enabledLog = preferences.getBoolean(ACTION_DEBUG, false);
        DebugInfo log = new DebugInfo(title, percent, content);
        Log.d(ACTION_DEBUG, log.getDebugInfo());
        if (enabledLog){
            File file = new File(getLogFileName(context));
            try (FileOutputStream outputStream = new FileOutputStream(file, true)){
                outputStream.write((gson.toJson(log) + '\n').getBytes());
            } catch (IOException e) {
                e.printStackTrace();
            }
            for (int i = listenerList.size() - 1; i >= 0 ; i--) {
                LogListener logListener = listenerList.get(i);
                if (logListener != null) logListener.newLog(log);
                else listenerList.remove(i);
            }
        }
    }

    public static List<DebugInfo> getLogs(Context context, LogListener listener){
        if (listener != null && !listenerList.contains(listener)) listenerList.add(listener);

        List<DebugInfo> logs = new ArrayList<>();

        File file = new File(getLogFileName(context));
        try(FileInputStream inputStream = new FileInputStream(file)){
            InputStreamReader reader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
            BufferedReader bufferedReader = new BufferedReader(reader);
            while (bufferedReader.ready()){
                DebugInfo debugInfo = gson.fromJson(bufferedReader.readLine(), DebugInfo.class);
                if (debugInfo != null) logs.add(debugInfo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return logs;
    }

    public static void closeLog(Context context){
        listenerList.clear();
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

    public interface LogListener{
        void newLog(DebugInfo log);
    }
}
