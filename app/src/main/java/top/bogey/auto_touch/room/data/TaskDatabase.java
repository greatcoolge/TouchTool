package top.bogey.auto_touch.room.data;

import android.content.Context;

import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.TypeConverters;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import top.bogey.auto_touch.room.bean.Task;

@Database(entities = {Task.class}, version = 1, exportSchema = false)
@TypeConverters({MyTypeConverters.class})
public abstract class TaskDatabase extends RoomDatabase {
    private static final String DB_NAME = "TASK_DB";
    private static volatile TaskDatabase instance;
    public static ThreadPoolExecutor service;
    static synchronized TaskDatabase getInstance(Context context){
        if (instance == null) instance = create(context);
        return instance;
    }

    private static TaskDatabase create(final Context context){
        service = new ThreadPoolExecutor(1, 5, 60L, TimeUnit.SECONDS, new ArrayBlockingQueue<>(20));
        return Room.databaseBuilder(context, TaskDatabase.class, DB_NAME).build();
    }

    public abstract TaskDao getTaskDao();
}
