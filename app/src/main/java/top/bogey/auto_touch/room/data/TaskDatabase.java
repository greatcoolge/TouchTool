package top.bogey.auto_touch.room.data;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;

import androidx.annotation.NonNull;
import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.TypeConverters;
import androidx.room.migration.Migration;
import androidx.sqlite.db.SupportSQLiteDatabase;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.util.AppUtil;

@Database(entities = {Task.class}, version = 2, exportSchema = false)
@TypeConverters({MyTypeConverters.class})
public abstract class TaskDatabase extends RoomDatabase {
    private static final String DB_NAME = "TASK_DB";
    private static volatile TaskDatabase instance;
    public static final ThreadPoolExecutor service = new ThreadPoolExecutor(1, 5, 60L, TimeUnit.SECONDS, new ArrayBlockingQueue<>(20));

    static synchronized TaskDatabase getInstance(Context context){
        if (instance == null) instance = create(context);
        return instance;
    }

    private static TaskDatabase create(final Context context){
        Migration MIGRATION_1_2 = new Migration(1, 2) {
            @Override
            public void migrate(@NonNull SupportSQLiteDatabase database) {
                Cursor cursor = database.query("select id, actions from Task");
                if (cursor.moveToFirst()){
                    do {
                        String id = cursor.getString(0);
                        String json = cursor.getString(1);
                        List<Action> actions = MyTypeConverters.fromString(json);
                        for (Action action : actions) {
                            List<Node> targets = action.getTargets();
                            for (Node target : targets) {
                                target.setTime(100);
                                if (target.getType() == NodeType.POS){
                                    List<Pos> posList = new ArrayList<>();
                                    List<Pos> poses = target.getPoses();
                                    for (Pos pos : poses) {
                                        posList.add(AppUtil.px2percent(context, pos));
                                    }
                                    target.setPoses(posList);
                                }
                            }
                        }
                        String result = MyTypeConverters.fromAction(actions);
                        ContentValues values = new ContentValues();
                        values.put("actions", result);
                        String[] where = {id};
                        database.update("Task", SQLiteDatabase.CONFLICT_NONE, values, "id=?", where);
                    } while (cursor.moveToNext());
                }
            }
        };

        return Room.databaseBuilder(context, TaskDatabase.class, DB_NAME)
                .addMigrations(MIGRATION_1_2)
                .build();
    }

    public abstract TaskDao getTaskDao();
}
