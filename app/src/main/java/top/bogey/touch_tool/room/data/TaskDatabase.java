package top.bogey.touch_tool.room.data;

import android.content.Context;

import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.TypeConverters;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import top.bogey.touch_tool.room.bean.Task;

@Database(entities = {Task.class}, version = 1, exportSchema = false)
@TypeConverters({CustomTypeConverts.class})
public abstract class TaskDatabase extends RoomDatabase {
    private static final String DB_NAME = "TASKS_DB";
    private static volatile TaskDatabase instance;
    public static final ThreadPoolExecutor service = new ThreadPoolExecutor(2, 5, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<>(20));

    static synchronized TaskDatabase getInstance(Context context){
        if (instance == null) instance = create(context);
        return instance;
    }

    private static TaskDatabase create(final Context context){
        return Room.databaseBuilder(context, TaskDatabase.class, DB_NAME)
                //.addMigrations()
                .build();
    }

    public abstract TaskDao getTaskDao();
}
