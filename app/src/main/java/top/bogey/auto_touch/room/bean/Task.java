package top.bogey.auto_touch.room.bean;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import java.util.ArrayList;
import java.util.List;

@Entity
public class Task implements Cloneable {
    @PrimaryKey(autoGenerate = true)
    public int id;
    public String pkgName;
    public String title;
    public int groupId = 0;
    public List<Action> actions;
    public TaskStatus taskStatus = TaskStatus.CLOSED;

    @NonNull
    @Override
    public Task clone(){
        try {
            Task clone = (Task) super.clone();
            if (actions != null){ clone.actions = new ArrayList<>(this.actions); }
            return clone;
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return new Task();
        }
    }
}
