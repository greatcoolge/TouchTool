package top.bogey.auto_touch.room.data;

import android.content.Context;

import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.TypeConverters;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import top.bogey.auto_touch.room.bean.Task;

@Database(entities = {Task.class}, version = 1, exportSchema = false)
@TypeConverters({MyTypeConverters.class})
public abstract class TaskDatabase extends RoomDatabase {
    private static final String DB_NAME = "TASK_DB";
    private static volatile TaskDatabase instance;
    public static final ExecutorService service = Executors.newFixedThreadPool(5);

    static synchronized TaskDatabase getInstance(Context context){
        if (instance == null) instance = create(context);
        return instance;
    }

    private static TaskDatabase create(final Context context){
        return Room.databaseBuilder(context, TaskDatabase.class, DB_NAME).build();
    }

    public abstract TaskDao getTaskDao();
}
