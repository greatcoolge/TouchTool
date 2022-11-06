package top.bogey.touch_tool.database.data;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.ActionType;
import top.bogey.touch_tool.database.bean.action.NumberAction;
import top.bogey.touch_tool.ui.setting.RunningUtils;
import top.bogey.touch_tool.utils.TaskRunningCallback;

public class TaskRunnable implements Runnable {
    private final MainAccessibilityService service;
    private final Task task;
    private final TaskRunningInfo runningInfo;

    public TaskRunnable(Task task, MainAccessibilityService service, String pkgName) {
        this.task = task;
        this.service = service;
        runningInfo = new TaskRunningInfo(this, pkgName, task.getLength());
    }

    public void addCallback(TaskRunningCallback callback) {
        runningInfo.addCallback(callback);
    }

    public void addCallbacks(List<TaskRunningCallback> callbacks){
        runningInfo.addCallbacks(callbacks);
    }

    public int getTaskPercent(){
        return runningInfo.getTaskPercent();
    }

    public Task getTask() {
        return task;
    }

    public void stop(boolean force) {
        if (force) runningInfo.setRunning(false);
        else if (task.isAcrossAppTask()) runningInfo.setRunning(true);
        runningInfo.setRunning(false);
    }

    public boolean isRunning() {
        return runningInfo.isRunning();
    }

    @Override
    public void run() {
        runningInfo.onStart(this);
        boolean result = runTask(task, service, runningInfo);
        runningInfo.onEnd(this, result);
        RunningUtils.run(service, task, runningInfo.getPkgName(), result);
        runningInfo.setRunning(false);
    }

    public static boolean runTask(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        // 获取任务中有效的行为
        List<Behavior> behaviors = new ArrayList<>();
        for (Behavior behavior : task.getBehaviors()) {
            if (behavior.isEnable()) behaviors.add(behavior);
        }

        boolean result = true;
        Behavior runBehavior = behaviors.remove(0);
        while (runBehavior != null && runningInfo.isRunning()) {
            List<Action> actions = runBehavior.getActions();
            Action condition = runBehavior.getCondition();
            switch (runBehavior.getActionMode()) {

                case CONDITION:
                    Action firstAction = actions.get(0);
                    Action secondAction = actions.size() > 1 ? actions.get(1) : null;
                    if (condition == null || condition.checkCondition(service)) {
                        result &= firstAction.doAction(task, service, runningInfo);
                        if (secondAction != null) runningInfo.addProgress(task, secondAction, true);
                    } else {
                        if (secondAction != null) {
                            result &= secondAction.doAction(task, service, runningInfo);
                        } else result = false;
                        runningInfo.addProgress(task, firstAction, true);
                    }
                    break;

                case LOOP:
                    int finishTimes = 0;
                    while (finishTimes < runBehavior.getTimes() && runningInfo.isRunning()) {
                        boolean flag = true;
                        for (int i = 0; i < actions.size(); i++) {
                            flag &= actions.get(i).doAction(task, service, runningInfo);
                        }
                        if (condition != null) {
                            if (condition.getType() == ActionType.NUMBER)
                                ((NumberAction) condition).addCurrNum(flag);
                            if (condition.getType() != ActionType.NULL && condition.checkCondition(service))
                                break;
                        }
                        finishTimes++;
                    }
                    // 有结束条件, 循环却运行到末尾，代表循环执行失败
                    if (condition != null && condition.getType() != ActionType.NULL && finishTimes == runBehavior.getTimes()) {
                        result = false;
                    }
                    break;


                case PARALLEL:
                    CountDownLatch latch = new CountDownLatch(actions.size());
                    for (Action action : actions) {
                        service.taskService.submit(() -> {
                            boolean flag = action.doAction(task, service, runningInfo);
                            if (condition.getType() == ActionType.NUMBER) ((NumberAction) condition).addCurrNum(flag);
                            latch.countDown();
                        });
                    }
                    try {
                        latch.await();
                        result = condition.checkCondition(service);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    break;
            }

            if (behaviors.size() > 0) runBehavior = behaviors.remove(0);
            else runBehavior = null;
        }

        return result;
    }


}
