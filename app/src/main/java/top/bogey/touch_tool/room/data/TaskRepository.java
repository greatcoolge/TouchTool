package top.bogey.touch_tool.room.data;

import android.content.Context;

import androidx.lifecycle.LiveData;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.room.bean.node.TaskNode;

public class TaskRepository {
    private static TaskRepository repository;
    private final TaskDao taskDao;

    public TaskRepository(Context context) {
        TaskDatabase database = TaskDatabase.getInstance(context.getApplicationContext());
        taskDao = database.getTaskDao();
    }

    public static TaskRepository getInstance(Context context) {
        if (repository == null) repository = new TaskRepository(context);
        return repository;
    }

    public List<Task> getAllTasks() {
        Future<List<Task>> future = TaskDatabase.service.submit(taskDao::getAllTasks);
        try {
            return future.get(1000, TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            e.printStackTrace();
        }
        return null;
    }

    public List<Task> getTasksByPackageName(String pkgName) {
        Future<List<Task>> future = TaskDatabase.service.submit(() -> taskDao.getTasksByPackageName(pkgName));
        try {
            return future.get(1000, TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    public List<Task> getTasksById(String id) {
        Future<List<Task>> future = TaskDatabase.service.submit(() -> taskDao.getTasksById(id));
        try {
            return future.get(1000, TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            e.printStackTrace();
        }
        return null;
    }

    public Task getTaskById(String id) {
        List<Task> tasks = getTasksById(id);
        if (tasks != null && tasks.size() > 0) return tasks.get(0);
        return null;
    }

    public List<Task> getTasksByStatus(TaskStatus status) {
        Future<List<Task>> future = TaskDatabase.service.submit(() -> taskDao.getTasksByStatus(status));
        try {
            return future.get(1000, TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            e.printStackTrace();
        }
        return null;
    }

    public LiveData<List<Task>> getTasksLiveByPackageName(String pkgName) {
        return taskDao.getTasksLiveByPackageName(pkgName);
    }

    public LiveData<List<TaskNode.TaskGroup>> getTaskGroupsLive() {
        return taskDao.getTaskGroupsLive();
    }

    public List<TaskNode.TaskGroup> getTaskGroups() {
        Future<List<TaskNode.TaskGroup>> future = TaskDatabase.service.submit(taskDao::getTaskGroups);
        try {
            return future.get(1000, TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void saveTask(Task task) {
        saveTask(task, false);
    }

    public void saveTask(Task task, boolean baseChanged) {
        TaskDatabase.service.execute(() -> taskDao.insert(task));
        MainAccessibilityService service = MainApplication.getService();
        if (service != null && baseChanged) {
            if (task.getStatus() == TaskStatus.TIME) service.addWork(task);
            else service.removeWork(task);
        }
    }

    public void saveTask(List<Task> tasks) {
        TaskDatabase.service.execute(() -> taskDao.insert(tasks));
        MainAccessibilityService service = MainApplication.getService();
        if (service != null) {
            for (Task task : tasks) {
                if (task.getStatus() == TaskStatus.TIME) service.addWork(task);
            }
        }
    }

    public void deleteTask(Task task) {
        TaskDatabase.service.execute(() -> taskDao.delete(task));
        MainAccessibilityService service = MainApplication.getService();
        if (service != null && task.getStatus() == TaskStatus.TIME) {
            service.removeWork(task);
        }
    }
}
