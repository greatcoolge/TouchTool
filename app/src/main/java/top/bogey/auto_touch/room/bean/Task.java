package top.bogey.auto_touch.room.bean;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
public class Task {
    @NonNull
    @PrimaryKey
    private String id;
    private String pkgName;

    private String title;
    private List<Action> actions = new ArrayList<>();
    private TaskStatus status = TaskStatus.CLOSED;

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
        status =task.getStatus();
    }

    @NonNull
    public String getId() {
        return id;
    }

    public void setId(@NonNull String id) {
        this.id = id;
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
}
