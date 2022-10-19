package top.bogey.touch_tool.room.bean;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import top.bogey.touch_tool.utils.AppUtils;

@Entity
public class Task {
    @NonNull
    @PrimaryKey
    private final String id;
    private String pkgName;
    private String title = "";
    private List<Action> actions = new ArrayList<>();
    private TaskStatus status = TaskStatus.CLOSED;
    private long time = AppUtils.mergeDateTime(System.currentTimeMillis(), System.currentTimeMillis());

    @Ignore
    public Task() {
        id = UUID.randomUUID().toString();
    }

    public Task(@NonNull String id) {
        this.id = id;
    }

    @Ignore
    public Task(Task task){
        id = UUID.randomUUID().toString();
        pkgName = "";

        title = task.getTitle();
        actions.addAll(task.getActions());
        status = task.getStatus();
        time = task.getTime();
    }

    @NonNull
    public String getId() {
        return id;
    }

    public String getPkgName() {
        return pkgName;
    }

    public void setPkgName(String pkgName) {
        this.pkgName = pkgName;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public List<Action> getActions() {
        return actions;
    }

    public void setActions(List<Action> actions) {
        this.actions = actions;
    }

    public TaskStatus getStatus() {
        return status;
    }

    public void setStatus(TaskStatus status) {
        this.status = status;
    }

    public long getTime() {
        return time;
    }

    public void setTime(long time) {
        this.time = time;
    }
}
