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
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.LogUtils;
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
        boolean result = runTask(task, task, service, runningInfo);
        runningInfo.onEnd(this, result);
        LogUtils.run(service, task, runningInfo.getPkgName(), result);
        runningInfo.setRunning(false);
    }

    public static boolean runTask(Task baseTask, Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        // 获取任务中有效的行为
        List<Behavior> behaviors = new ArrayList<>();
        for (Behavior behavior : task.getBehaviors()) {
            if (behavior.isEnable()) behaviors.add(behavior);
        }

        boolean result = true;
        Behavior runBehavior = behaviors.remove(0);
        while (runBehavior != null) {
            if (!runningInfo.isRunning()) return false;

            List<Action> actions = runBehavior.getActions();
            Action condition = runBehavior.getCondition();
            switch (runBehavior.getBehaviorMode()) {

                case CONDITION:
                    Action firstAction = actions.get(0);
                    Action secondAction = actions.size() > 1 ? actions.get(1) : null;
                    if (condition == null || condition.checkCondition(service)) {
                        result &= doAction(service, baseTask, runBehavior, firstAction, runningInfo);
                        if (secondAction != null) runningInfo.addProgress(baseTask, secondAction, true);
                    } else {
                        if (secondAction != null) {
                            result &= doAction(service, baseTask, runBehavior, secondAction, runningInfo);
                        } else {
                            result = false;
                            runningInfo.addProgress(baseTask, firstAction, true);
                            LogUtils.log(LogLevel.LOW, service, false, baseTask, runningInfo, condition.getConditionContent(service, baseTask, runBehavior));
                        }
                    }
                    break;

                case LOOP:
                    int finishTimes = 0;
                    while (finishTimes < runBehavior.getTimes()) {
                        if (!runningInfo.isRunning()) return false;

                        boolean flag = true;
                        for (int i = 0; i < actions.size(); i++) {
                            flag &= doAction(service, baseTask, runBehavior, actions.get(i), runningInfo);
                        }
                        if (condition != null) {
                            if (condition.getType() == ActionType.NUMBER){
                                ((NumberAction) condition).addCurrNum(flag);
                                LogUtils.log(LogLevel.LOW, service, condition.checkCondition(service), baseTask, runningInfo, condition.getConditionContent(service, baseTask, runBehavior));
                            }
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
                        Behavior finalRunBehavior = runBehavior;
                        service.taskService.submit(() -> {
                            boolean flag = doAction(service, baseTask, finalRunBehavior, action, runningInfo);
                            if (condition.getType() == ActionType.NUMBER) ((NumberAction) condition).addCurrNum(flag);
                            latch.countDown();
                        });
                    }
                    try {
                        latch.await();
                        result = condition.checkCondition(service);
                        LogUtils.log(LogLevel.LOW, service, result, baseTask, runningInfo, condition.getConditionContent(service, baseTask, runBehavior));
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

    private static boolean doAction(MainAccessibilityService service, Task baseTask, Behavior behavior, Action action, TaskRunningInfo runningInfo){
        boolean flag = action.doAction(baseTask, service, runningInfo);
        runningInfo.addProgress(baseTask, action, false);
        LogUtils.log(action.getType() == ActionType.DELAY || !flag ? LogLevel.LOW : LogLevel.HIGH, service, flag, baseTask, runningInfo, action.getDescription(service, baseTask, behavior));
        return flag;
    }
}
