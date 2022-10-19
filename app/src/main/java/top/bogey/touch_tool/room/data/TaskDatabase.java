package top.bogey.touch_tool.room.data;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.TypeConverters;
import androidx.room.migration.Migration;
import androidx.sqlite.db.SupportSQLiteDatabase;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import top.bogey.touch_tool.room.bean.Task;

@Database(entities = {Task.class}, version = 2, exportSchema = false)
@TypeConverters({CustomTypeConverts.class})
public abstract class TaskDatabase extends RoomDatabase {
    private static final String DB_NAME = "TASKS_DB";
    private static volatile TaskDatabase instance;
    public static final ThreadPoolExecutor service = new ThreadPoolExecutor(2, 5, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<>(20));

    private static final Migration MIGRATION_1_2 = new Migration(1, 2) {
        @Override
        public void migrate(@NonNull SupportSQLiteDatabase database) {
            database.execSQL("alter table Task add column time integer not null default " + System.currentTimeMillis());
        }
    };

    static synchronized TaskDatabase getInstance(Context context){
        if (instance == null) instance = create(context);
        return instance;
    }

    private static TaskDatabase create(final Context context){
        return Room.databaseBuilder(context, TaskDatabase.class, DB_NAME)
                .addMigrations(MIGRATION_1_2)
                .build();
    }

    public abstract TaskDao getTaskDao();
}
