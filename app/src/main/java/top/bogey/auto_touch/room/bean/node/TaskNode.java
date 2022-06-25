package top.bogey.auto_touch.room.bean.node;

import androidx.annotation.NonNull;

import java.util.HashMap;
import java.util.Map;

import top.bogey.auto_touch.room.bean.Task;

public class TaskNode extends Node{
    public TaskNode(TaskInfo value) {
        super(NodeType.TASK, value);
    }

    @Override
    public TaskInfo getValue() {
        return (TaskInfo) value;
    }

    @Override
    public boolean isValid() {
        return getValue() != null;
    }

    @Override
    public boolean checkNode(Object obj) {
        Map<String, Task> taskMap = toTaskMap(obj);
        Task task = taskMap.get(getValue().getId());
        return task != null;
    }

    @Override
    public Object getNodeTarget(Object obj) {
        Map<String, Task> taskMap = toTaskMap(obj);
        return taskMap.get(getValue().getId());
    }

    private static Map<String, Task> toTaskMap(Object obj){
        Map<String, Task> map = new HashMap<>();
        if (obj instanceof HashMap<?, ?>){
            for (Map.Entry<?, ?> entry : ((Map<?, ?>) obj).entrySet()) {
                map.put((String) entry.getKey(), (Task) entry.getValue());
            }
        }
        return map;
    }

    public static class TaskInfo{
        private String id;
        private String title;

        public TaskInfo(String id, String title) {
            this.id = id;
            this.title = title;
        }

        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        @NonNull
        @Override
        public String toString() {
            return title;
        }
    }

    public static class TaskGroup{
        private String pkgName;
        private int count;

        public String getPkgName() {
            return pkgName;
        }

        public void setPkgName(String pkgName) {
            this.pkgName = pkgName;
        }

        public int getCount() {
            return count;
        }

        public void setCount(int count) {
            this.count = count;
        }
    }
}
