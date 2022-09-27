package top.bogey.touch_tool.ui.setting;

public class RunningInfo {
    private final String taskId;
    private int doCount = 0;
    private int successCount = 0;

    public RunningInfo(String taskId) {
        this.taskId = taskId;
    }

    public String getTaskId() {
        return taskId;
    }

    public int getDoCount() {
        return doCount;
    }

    public int getSuccessCount() {
        return successCount;
    }

    public void addCount(boolean success){
        doCount++;
        if (success) successCount++;
    }
}
