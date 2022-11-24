package top.bogey.touch_tool.database.data;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.ActionType;
import top.bogey.touch_tool.database.bean.action.NumberAction;
import top.bogey.touch_tool.database.bean.action.TaskAction;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.LogUtils;
import top.bogey.touch_tool.utils.AppUtils;
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

    public void addCallbacks(List<TaskRunningCallback> callbacks) {
        runningInfo.addCallbacks(callbacks);
    }

    public Task getTask() {
        return task;
    }

    public void stop(boolean force) {
        if (force) runningInfo.setRunning(task, false);
        else if (task.isAcrossAppTask()) runningInfo.setRunning(task, true);
        runningInfo.setRunning(task, false);
    }

    public boolean isRunning() {
        return runningInfo.isRunning(task);
    }

    @Override
    public void run() {
        boolean result = false;
        try {
            runningInfo.onStart(this);
            result = runTask(task, task, service, runningInfo);
        } catch (Exception e) {
            e.printStackTrace();
            LogUtils.log(LogLevel.HIGH, e.toString());
        } finally {
            runningInfo.onEnd(this, result);
            LogUtils.run(service, task, runningInfo.getPkgName(), result);
        }
    }

    public static boolean runTask(Task baseTask, Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        runningInfo.setRunning(task, true);

        // 获取任务中有效的行为
        List<Behavior> behaviors = new ArrayList<>();
        for (Behavior behavior : task.getBehaviors()) {
            if (behavior.isEnable()) behaviors.add(behavior);
        }

        boolean result = true;
        Behavior runBehavior = behaviors.remove(0);
        while (runBehavior != null) {
            if (!(runningInfo.isRunning(baseTask) && runningInfo.isRunning(task))) return false;

            List<Action> actions = runBehavior.getActions();
            // 条件换成新对象，防止重入
            Action condition = AppUtils.copy(runBehavior.getCondition());
            switch (runBehavior.getBehaviorMode()) {

                case CONDITION:
                    Action firstAction = actions.get(0);
                    Action secondAction = actions.size() > 1 ? actions.get(1) : null;
                    if (condition == null || condition.checkCondition(service)) {
                        result &= doAction(service, baseTask, task, runBehavior, firstAction, runningInfo);
                        if (secondAction != null) runningInfo.addProgress(baseTask, secondAction, true);
                    } else {
                        if (secondAction != null) {
                            result &= doAction(service, baseTask, task, runBehavior, secondAction, runningInfo);
                        } else {
                            result = false;
                            runningInfo.addProgress(baseTask, firstAction, true);
                            LogUtils.log(LogLevel.LOW, service.getString(R.string.log_run_behavior_condition_fail, condition.getConditionContent(service, baseTask, runBehavior), task.getTitle()));
                        }
                    }
                    break;

                case LOOP:
                    int finishTimes = 0;
                    while (finishTimes < runBehavior.getTimes()) {
                        if (!(runningInfo.isRunning(baseTask) && runningInfo.isRunning(task))) return false;

                        boolean flag = true;
                        for (int i = 0; i < actions.size(); i++) {
                            flag &= doAction(service, baseTask, task, runBehavior, actions.get(i), runningInfo);
                        }
                        if (condition != null) {
                            if (condition.getType() == ActionType.NUMBER) {
                                ((NumberAction) condition).addCurrNum(flag);
                            }
                            if (condition.getType() != ActionType.NULL && condition.checkCondition(service))
                                break;
                        }
                        finishTimes++;
                    }
                    // 有结束条件, 循环却运行到末尾，代表循环执行失败
                    if (condition != null && condition.getType() != ActionType.NULL && finishTimes == runBehavior.getTimes()) {
                        result = false;
                        LogUtils.log(LogLevel.LOW, service.getString(R.string.log_run_behavior_condition_fail, condition.getConditionContent(service, baseTask, runBehavior), task.getTitle()));
                    }
                    break;

                case PARALLEL:
                    if (condition.getType() == ActionType.NUMBER) {
                        int start = runningInfo.getProgress();
                        NumberAction numberAction = (NumberAction) condition;
                        CountDownLatch latch = new CountDownLatch(numberAction.getTargetNum());
                        Behavior finalRunBehavior = runBehavior;
                        List<Action> actionList = new ArrayList<>();
                        for (Action action : actions) {
                            Action finalAction = AppUtils.copy(action);
                            service.taskService.submit(() -> {
                                boolean flag = doFinalAction(service, baseTask, task, finalRunBehavior, finalAction, runningInfo);
                                if (condition.getType() == ActionType.NUMBER) numberAction.addCurrNum(flag);
                                latch.countDown();
                            });
                            actionList.add(finalAction);
                        }
                        try {
                            latch.await();
                            result = condition.checkCondition(service);
                            actionList.forEach(action -> {
                                if (action.getType() == ActionType.TASK) {
                                    Task subTask = ((TaskAction) action).getSubTask();
                                    if (subTask != null) runningInfo.setRunning(subTask, false);
                                }
                            });

                            if (!result) LogUtils.log(LogLevel.LOW, service.getString(R.string.log_run_behavior_condition_fail, condition.getConditionContent(service, baseTask, runBehavior), task.getTitle()));
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                        runningInfo.jumpProgress(start, baseTask, actions);
                    }
                    break;
            }

            if (behaviors.size() > 0) runBehavior = behaviors.remove(0);
            else runBehavior = null;
        }

        runningInfo.setRunning(task, false);
        return result;
    }

    private static boolean doAction(MainAccessibilityService service, Task baseTask, Task currTask, Behavior behavior, Action action, TaskRunningInfo runningInfo) {
        Action finalAction = AppUtils.copy(action);
        return doFinalAction(service, baseTask, currTask, behavior, finalAction, runningInfo);
    }

    // 因为有并行执行，所有action都需要是单独的动作，防止重入
    private static boolean doFinalAction(MainAccessibilityService service, Task baseTask, Task currTask, Behavior behavior, Action action, TaskRunningInfo runningInfo) {
        boolean flag = action.doAction(baseTask, service, runningInfo);
        addProgress(service, baseTask, currTask, behavior, action, runningInfo, flag);
        return flag;
    }

    private synchronized static void addProgress(MainAccessibilityService service, Task baseTask, Task currTask, Behavior behavior, Action action, TaskRunningInfo runningInfo, boolean flag) {
        runningInfo.addProgress(baseTask, action, false);
        LogUtils.log(action.getType() == ActionType.DELAY || !flag ? LogLevel.LOW : LogLevel.HIGH, service, flag, baseTask, currTask, runningInfo, action.getDescription(service, baseTask, behavior));
    }
}
