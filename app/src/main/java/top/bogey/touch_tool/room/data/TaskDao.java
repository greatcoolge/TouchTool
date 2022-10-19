package top.bogey.touch_tool.room.data;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import java.util.List;

import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.room.bean.node.TaskNode;

@Dao
public interface TaskDao {
    @Query("select * from Task")
    List<Task> getAllTasks();

    @Query("select * from Task where pkgName=:pkgName")
    List<Task> getTasksByPackageName(String pkgName);

    @Query("select * from Task where id = :id")
    List<Task> getTasksById(String id);

    @Query("select * from Task where pkgName=:pkgName")
    LiveData<List<Task>> getTasksLiveByPackageName(String pkgName);

    @Query("select pkgName, count(*) as count from Task group by pkgName")
    LiveData<List<TaskNode.TaskGroup>> getTaskGroupsLive();

    @Query("select pkgName, count(*) as count from Task group by pkgName")
    List<TaskNode.TaskGroup> getTaskGroups();

    @Query("select * from Task where status = :status")
    List<Task> getTasksByStatus(TaskStatus status);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(Task task);

    @Insert(onConflict = OnConflictStrategy.IGNORE)
    void insert(List<Task> tasks);

    @Delete
    void delete(Task task);
}
