package top.bogey.touch_tool.room.data;

import android.content.Context;

import androidx.lifecycle.LiveData;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.node.TaskNode;

public class TaskRepository {
    private final TaskDao taskDao;

    public TaskRepository(Context context){
        TaskDatabase database = TaskDatabase.getInstance(context.getApplicationContext());
        taskDao = database.getTaskDao();
    }

    public List<Task> getAllTasks(){
        Future<List<Task>> future = TaskDatabase.service.submit(taskDao::getAllTasks);
        try {
            return future.get();
        } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
        }
        return null;
    }

    public List<Task> getTasksByPackageName(String pkgName){
        Future<List<Task>> future = TaskDatabase.service.submit(() -> taskDao.getTasksByPackageName(pkgName));
        try {
            return future.get();
        } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
//            return getTasksByPackageName(pkgName);
        }
        return new ArrayList<>();
    }

    public List<Task> getTasksById(String id){
        Future<List<Task>> future = TaskDatabase.service.submit(() -> taskDao.getTasksById(id));
        try {
            return future.get();
        } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
        }
        return null;
    }

    public LiveData<List<Task>> getTasksLiveByPackageName(String pkgName){
        return taskDao.getTasksLiveByPackageName(pkgName);
    }

    public LiveData<List<TaskNode.TaskGroup>> getTaskGroupsLive(){
        return taskDao.getTaskGroupsLive();
    }

    public void saveTask(Task task){
        TaskDatabase.service.execute(() -> taskDao.insert(task));
    }

    public void saveTask(List<Task> tasks){
        TaskDatabase.service.execute(() -> taskDao.insert(tasks));
    }

    public void deleteTask(Task task) {
        TaskDatabase.service.execute(() -> taskDao.delete(task));
    }
}
