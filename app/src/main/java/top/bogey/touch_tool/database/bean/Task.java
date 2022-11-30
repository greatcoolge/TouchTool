package top.bogey.touch_tool.database.bean;

import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.ActionType;
import top.bogey.touch_tool.database.bean.action.TaskAction;
import top.bogey.touch_tool.database.bean.condition.TaskCondition;

public class Task implements Parcelable {
    // 任务ID
    private String id;
    // 任务类型
    private TaskType type;
    // 任务标题
    private String title = "";
    // 是否跨应用
    private boolean acrossApp;

    // 适用的应用
    private List<String> pkgNames;
    // 任务配置
    private List<Behavior> behaviors;
    // 执行条件
    private TaskCondition condition;
    // 内嵌任务
    private List<Task> subTasks;

    // 任务长度
    private transient int length;

    public Task(Context context) {
        id = UUID.randomUUID().toString();
        type = TaskType.CLOSED;
        title = context.getString(R.string.task_title_default);
        acrossApp = false;
    }

    public Task(Behavior behavior) {
        id = UUID.randomUUID().toString();
        behaviors = Collections.singletonList(behavior);
    }

    protected Task(Parcel in) {
        id = in.readString();
        type = in.readParcelable(TaskType.class.getClassLoader());
        title = in.readString();
        acrossApp = in.readByte() != 0;
        pkgNames = in.createStringArrayList();
        behaviors = in.createTypedArrayList(Behavior.CREATOR);
        condition = in.readParcelable(TaskCondition.class.getClassLoader());
        subTasks = in.createTypedArrayList(CREATOR);
    }

    public int getLength() {
        if (length == 0) {
            for (Behavior behavior : behaviors) {
                if (behavior.isEnable()) {
                    int behaviorLength = 0;
                    for (Action action : behavior.getActions()) {
                        if (action.getType() == ActionType.TASK) {
                            Task task = getSubTaskById(((TaskAction) action).getId());
                            if (task != null) {
                                behaviorLength += task.getLength();
                            }
                        }
                        behaviorLength++;
                    }
                    if (behavior.getBehaviorMode() == BehaviorMode.LOOP) {
                        behaviorLength *= behavior.getTimes();
                    }
                    length += behaviorLength;
                }
            }
        }
        return length;
    }

    public String getDescription(Context context) {
        StringBuilder builder = new StringBuilder();
        if (behaviors != null) {
            for (Behavior behavior : behaviors) {
                builder.append(behavior.getTitle(context, this));
                builder.append("\n");
            }
        }
        return builder.toString().trim();
    }

    public Task getSubTaskById(String id) {
        if (subTasks == null) return null;
        for (Task task : subTasks) {
            if (task.getId().equals(id)) return task;
        }
        return null;
    }

    public void addSubTask(Task task) {
        if (subTasks == null) subTasks = new ArrayList<>();
        subTasks.add(task);
    }

    public void removeSubTask(Task task) {
        if (subTasks == null) return;
        for (int i = 0; i < subTasks.size(); i++) {
            if (subTasks.get(i).getId().equals(task.getId())) {
                subTasks.remove(i);
                break;
            }
        }
        if (subTasks.size() == 0) subTasks = null;
    }

    public void addBehavior(Behavior behavior) {
        if (behaviors == null) behaviors = new ArrayList<>();
        behaviors.add(behavior);
    }

    public void addBehavior(int index, Behavior behavior) {
        if (behaviors == null) behaviors = new ArrayList<>();
        behaviors.add(index, behavior);
    }

    public List<Task> getSafeSubTasks(String id) {
        if (subTasks == null || subTasks.isEmpty()) return null;
        List<Task> tasks = new ArrayList<>();
        for (Task subTask : subTasks) {
            // 不看自己
            if (subTask.getId().equals(id)) continue;
            if (!includeTaskAction(subTask, id)) {
                tasks.add(subTask);
            }
        }
        return tasks;
    }

    public boolean includeTaskAction(Task task, String id) {
        if (task == null) return false;
        List<Behavior> subTaskBehaviors = task.getBehaviors();
        if (subTaskBehaviors == null || subTaskBehaviors.isEmpty()) return false;
        for (Behavior behavior : subTaskBehaviors) {
            List<Action> actions = behavior.getActions();
            if (actions == null || actions.isEmpty()) continue;
            for (Action action : actions) {
                if (action.getType() == ActionType.TASK) {
                    TaskAction taskAction = (TaskAction) action;
                    if (taskAction.getId().equals(id)) return true;
                    else {
                        boolean result = includeTaskAction(getSubTaskById(taskAction.getId()), id);
                        if (result) return true;
                    }
                }
            }
        }
        return false;
    }

    public boolean includeCaptureAction() {
        List<Task> tasks = new ArrayList<>();
        tasks.add(this);
        if (subTasks != null) tasks.addAll(subTasks);
        for (Task task : tasks) {
            List<Behavior> taskBehaviors = task.getBehaviors();
            if (behaviors == null || behaviors.isEmpty()) continue;
            for (Behavior behavior : taskBehaviors) {
                List<Action> actions = behavior.getActions();
                if (actions == null || actions.isEmpty()) continue;
                for (Action action : actions) {
                    if (action.getType() == ActionType.IMAGE || action.getType() == ActionType.COLOR) return true;
                }
                Action action = behavior.getCondition();
                if (action != null && (action.getType() == ActionType.IMAGE || action.getType() == ActionType.COLOR)) return true;
            }
        }
        return false;
    }

    public boolean isAcrossAppTask() {
        return acrossApp || type == TaskType.IT_IS_TIME;
    }

    public static final Creator<Task> CREATOR = new Creator<Task>() {
        @Override
        public Task createFromParcel(Parcel in) {
            return new Task(in);
        }

        @Override
        public Task[] newArray(int size) {
            return new Task[size];
        }
    };

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public TaskType getType() {
        return type;
    }

    public void setType(TaskType type) {
        this.type = type;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public boolean isAcrossApp() {
        return acrossApp;
    }

    public void setAcrossApp(boolean acrossApp) {
        this.acrossApp = acrossApp;
    }

    public List<String> getPkgNames() {
        return pkgNames;
    }

    public void setPkgNames(List<String> pkgNames) {
        this.pkgNames = pkgNames;
    }

    public List<Behavior> getBehaviors() {
        return behaviors;
    }

    public void setBehaviors(List<Behavior> behaviors) {
        this.behaviors = behaviors;
    }

    public TaskCondition getCondition() {
        return condition;
    }

    public void setCondition(TaskCondition condition) {
        this.condition = condition;
    }

    public List<Task> getSubTasks() {
        return subTasks;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(id);
        dest.writeParcelable(type, flags);
        dest.writeString(title);
        dest.writeByte((byte) (acrossApp ? 1 : 0));

        if (type == TaskType.IT_IS_TIME) dest.writeStringList(null);
        else if (pkgNames != null && pkgNames.isEmpty()) dest.writeStringList(null);
        else dest.writeStringList(pkgNames);

        if (behaviors != null && behaviors.isEmpty()) dest.writeTypedList(null);
        else dest.writeTypedList(behaviors);

        if (type == TaskType.NEW_NOTIFICATION || type == TaskType.IT_IS_TIME) dest.writeParcelable(condition, flags);
        else dest.writeParcelable(null, flags);

        dest.writeTypedList(subTasks);
    }
}
