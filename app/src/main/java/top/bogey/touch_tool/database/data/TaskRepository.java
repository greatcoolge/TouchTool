package top.bogey.touch_tool.database.data;

import android.content.Context;
import android.os.Parcel;

import com.tencent.mmkv.MMKV;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskConfig;
import top.bogey.touch_tool.database.bean.TaskType;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.TaskChangedCallback;

public class TaskRepository {
    private static TaskRepository repository;
    private final static String TASK_DB = "task_db";
    private final static MMKV taskMMKV = MMKV.mmkvWithID(TASK_DB, MMKV.SINGLE_PROCESS_MODE, TASK_DB);

    private final static String TASK_CONFIG_DB = "task_config_db";
    private final static MMKV configMMKV = MMKV.mmkvWithID(TASK_CONFIG_DB, MMKV.SINGLE_PROCESS_MODE, TASK_CONFIG_DB);
    private final static String TASK_CONFIG_TAG = "task_config_tag";

    private List<Task> tasks;
    private boolean isChanged = true;

    private final List<TaskChangedCallback> callbacks = new ArrayList<>();

    public TaskRepository() {
    }

    public static TaskRepository getInstance() {
        if (repository == null) repository = new TaskRepository();
        return repository;
    }

    public List<Task> getAllTasks() {
        if (isChanged) {
            List<Task> tasks = new ArrayList<>();
            String[] keys = taskMMKV.allKeys();
            if (keys != null) {
                for (String key : keys) {
                    Task task = getTaskById(key);
                    if (task != null) {
                        tasks.add(task);
                    } else {
                        // 解析不出来的任务直接删掉
                        taskMMKV.remove(key);
                        configMMKV.remove(key);
                    }
                }
            }
            this.tasks = tasks;
            isChanged = false;
            return tasks;
        } else {
            return tasks;
        }
    }

    public List<Task> getTasksByTag(String tag) {
        List<Task> tasks = new ArrayList<>();
        String[] keys = configMMKV.allKeys();
        if (keys == null) return tasks;
        for (String key : keys) {
            if (!key.equals(TASK_CONFIG_TAG)) {
                TaskConfig taskConfig = configMMKV.decodeParcelable(key, TaskConfig.class);
                if (taskConfig != null){
                    if ((tag == null && taskConfig.getTag() == null) || (tag != null && tag.equals(taskConfig.getTag()))){
                        Task task = getTaskById(taskConfig.getId());
                        if (task != null) {
                            tasks.add(task);
                        }
                    }
                }
            }
        }
        return tasks;
    }

    public void addCallback(TaskChangedCallback callback) {
        if (callback != null && !callbacks.contains(callback)) callbacks.add(callback);
    }

    public void removeCallback(TaskChangedCallback callback) {
        callbacks.remove(callback);
    }

    public Task getTaskById(String id) {
        return taskMMKV.decodeParcelable(id, Task.class);
    }

    public List<Task> getTasksByPkgName(String pkgName) {
        List<Task> tasks = new ArrayList<>();
        for (Task task : getAllTasks()) {
            if (task.getPkgNames() != null && task.getPkgNames().contains(pkgName)) {
                tasks.add(task);
            }
        }
        return tasks;
    }

    public List<Task> getTasksByType(TaskType type) {
        List<Task> tasks = new ArrayList<>();
        for (Task task : getAllTasks()) {
            if (task.getType() != null && task.getType() == type) {
                tasks.add(task);
            }
        }
        return tasks;
    }

    public void saveTask(Task task) {
        Task originTask = getTaskById(task.getId());

        // 内容完全一致就不保存了
        if (Objects.equals(task, originTask)) return;

        // 如果与时间相关的值不相等，尝试更新定时任务
        if (originTask == null || !(task.getType() == originTask.getType() && Objects.equals(task.getCondition(), originTask.getCondition()))) {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null) {
                service.addWork(task);
            }
        }

        taskMMKV.encode(task.getId(), task);
        isChanged = true;

        TaskConfig config = configMMKV.decodeParcelable(task.getId(), TaskConfig.class, new TaskConfig(task.getId()));
        if (config != null) {
            config.setModifyTime(System.currentTimeMillis());
            configMMKV.encode(task.getId(), config);
        }

        callbacks.forEach(callback -> callback.onChanged(task));
    }

    public void deleteTask(Task task) {
        // 移除定时任务，尝试移除已激活的定时任务
        if (task.getType() == TaskType.IT_IS_TIME) {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null) {
                service.removeWork(task);
            }
        }

        taskMMKV.remove(task.getId());
        isChanged = true;

        configMMKV.remove(task.getId());

        callbacks.forEach(callback -> callback.onRemoved(task));
    }

    public TaskConfig getTaskConfig(String id) {
        return configMMKV.decodeParcelable(id, TaskConfig.class);
    }

    public void setTaskConfig(String id, String tag) {
        TaskConfig taskConfig = getTaskConfig(id);
        if (taskConfig != null) {
            taskConfig.setTag(tag);
            configMMKV.encode(id, taskConfig);
        }
    }

    public List<String> getTags(Context context) {
        List<String> tags = new ArrayList<>();
        byte[] bytes = configMMKV.decodeBytes(TASK_CONFIG_TAG);
        Parcel parcel = Parcel.obtain();
        if (bytes == null || bytes.length == 0) {
            tags = Arrays.asList(context.getString(R.string.tag_all), context.getString(R.string.tag_no));
            parcel.writeStringList(tags);
            configMMKV.encode(TASK_CONFIG_TAG, parcel.marshall());
        } else {
            parcel.unmarshall(bytes, 0, bytes.length);
            parcel.setDataPosition(0);
            parcel.readStringList(tags);
        }
        parcel.recycle();
        return tags;
    }

    public void addTag(Context context, String tag) {
        if (tag == null || tag.isEmpty()) return;
        List<String> tags = getTags(context);
        tags.add(tags.size() - 1, tag);
        Parcel parcel = Parcel.obtain();
        parcel.writeStringList(tags);
        configMMKV.encode(TASK_CONFIG_TAG, parcel.marshall());
        parcel.recycle();
    }

    public void removeTag(Context context, String tag) {
        if (tag == null || tag.isEmpty()) return;
        String[] keys = configMMKV.allKeys();
        if (keys == null) return;
        for (String key : keys) {
            if (!key.equals(TASK_CONFIG_TAG)) {
                TaskConfig taskConfig = configMMKV.decodeParcelable(key, TaskConfig.class);
                if (taskConfig != null && tag.equals(taskConfig.getTag())) {
                    setTaskConfig(taskConfig.getId(), null);
                }
            }
        }
        List<String> tags = getTags(context);
        tags.remove(tag);
        Parcel parcel = Parcel.obtain();
        parcel.writeStringList(tags);
        configMMKV.encode(TASK_CONFIG_TAG, parcel.marshall());
        parcel.recycle();
    }
}
