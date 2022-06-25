package top.bogey.auto_touch.room.data;

import android.graphics.Path;
import android.view.View;
import android.view.accessibility.AccessibilityNodeInfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.ActionMode;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.node.Node;
import top.bogey.auto_touch.room.bean.node.NodeType;
import top.bogey.auto_touch.room.bean.node.NumberNode;
import top.bogey.auto_touch.room.bean.node.TaskNode;
import top.bogey.auto_touch.ui.debug.DebugFloatView;
import top.bogey.auto_touch.utils.TaskCallback;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;

public class TaskRunnable implements Runnable{
    private boolean isRunning = true;

    private final MainAccessibilityService service;
    private final Task task;
    private final TaskCallback callback;

    private final TaskRepository repository;
    private final Map<String, Task> taskMap = new HashMap<>();
    private final Map<Node, Integer> taskNodeMap = new HashMap<>();
    private final int allPercent;
    private int percent = 0;

    public TaskRunnable(MainAccessibilityService service, Task task, TaskCallback callback) {
        this.service = service;
        this.task = task;
        this.callback = callback;

        repository = new TaskRepository(service);
        getAllTasks(taskMap, task);
        allPercent = getAllPercent(task);
    }

    public void stop() {
        isRunning = false;
    }

    public boolean isRunning(){
        return isRunning;
    }

    @Override
    public void run() {
        if (callback != null) callback.onStart();
        runTask(task);
        if (callback != null) callback.onEnd(isRunning);
    }

    private boolean runTask(Task task){
        // 获取任务中有效的动作
        List<Action> actions = new ArrayList<>();
        for (Action action : task.getActions()) {
            if (action.isEnable()) actions.add(action);
        }

        boolean result = true;
        Action runAction = actions.remove(0);
        while (runAction != null && isRunning()){
            switch (runAction.getActionMode()) {
                case CONDITION:
                    if (runAction.getCondition() == null || checkNode(runAction.getCondition())) {
                        result &= doAction(runAction, 0);
                        if (runAction.getTargets().size() > 1)
                            addTaskProgress(runAction.getTargets().get(1), true);
                    } else {
                        if (runAction.getTargets().size() > 1) result &= doAction(runAction, 1);
                        else result = false;
                        addTaskProgress(runAction.getTargets().get(0), true);
                    }
                    break;
                case LOOP:
                    int succeedTimes = 0;
                    int finishTimes = 0;
                    Node stop = runAction.getCondition();
                    while (finishTimes < runAction.getTimes() && isRunning()){
                        boolean flag = true;
                        for (int i = 0; i < runAction.getTargets().size(); i++) {
                            flag &= doAction(runAction, i);
                        }
                        if (flag) succeedTimes++;
                        if (stop.getType() != NodeType.NULL){
                            if (stop.getType() == NodeType.NUMBER && succeedTimes >= ((NumberNode) stop).getValue()){
                                break;
                            }
                            if ((stop.getType() == NodeType.TEXT || stop.getType() == NodeType.IMAGE) && checkNode(stop)) break;
                        }
                        finishTimes++;
                    }
                    // 有结束条件循环却一次都没成功，代表循环执行失败
                    if (stop.getType() != NodeType.NULL && succeedTimes == 0){
                        result = false;
                    }
                    break;
                case PARALLEL:
                    CountDownLatch latch = new CountDownLatch(((NumberNode) runAction.getCondition()).getValue());
                    Action finalRunAction = runAction;
                    for (int i = 0; i < runAction.getTargets().size(); i++) {
                        int index = i;
                        service.taskService.submit(() -> {
                            if (doAction(finalRunAction, index)) latch.countDown();
                        });
                    }
                    try {
                        if (!latch.await(60, TimeUnit.SECONDS)) stop();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    break;
            }

            if (actions.size() > 0) runAction = actions.remove(0);
            else runAction = null;
        }

        return result & isRunning();
    }

    private void sleep(int time){
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private boolean doAction(Action action, int index){
        List<Node> targets = action.getTargets();
        if (targets.size() > index){
            boolean result = false;
            Node target = targets.get(index);
            addDebugTips("["+ task.getTitle() +"]["+ percent +"]" + action.getTargetTitle(service, target));
            Object nodeTarget = getNodeTarget(target);
            if (nodeTarget != null){
                result = true;
                int randomTime = target.getTimeArea().getRandomTime();
                switch (target.getType()) {
                    case DELAY:
                        sleep((Integer) nodeTarget);
                        break;
                    case TEXT:
                        AccessibilityNodeInfo nodeInfo = (AccessibilityNodeInfo) nodeTarget;
                        if (target.getTimeArea().getRealMax() <= 100){
                            nodeInfo.performAction(AccessibilityNodeInfo.ACTION_CLICK);
                        } else {
                            nodeInfo.performAction(AccessibilityNodeInfo.ACTION_LONG_CLICK);
                        }
                        sleep(randomTime);
                        break;
                    case IMAGE:
                    case TOUCH:
                        Path path = (Path) nodeTarget;
                        service.runGesture(path, randomTime, null);
                        sleep(randomTime);
                        break;
                    case COLOR:
                        Path[] paths = (Path[]) nodeTarget;
                        for (Path path1 : paths) {
                            service.runGesture(path1, 100, null);
                            sleep(randomTime);
                        }
                        break;
                    case KEY:
                        service.performGlobalAction((Integer) nodeTarget);
                        break;
                    case TASK:
                        result = runTask((Task) nodeTarget);
                        break;
                }
            }
            addTaskProgress(target, nodeTarget == null);
            return result;
        }
        return false;
    }

    private void addDebugTips(String tips){
        View view = EasyFloat.getView(DebugFloatView.class.getCanonicalName());
        if (view != null){
            DebugFloatView debugFloatView = (DebugFloatView) view;
            debugFloatView.addTips(tips);
        }
    }

    private boolean checkNode(Node node){
        switch (node.getType()) {
            case TEXT:
                return node.checkNode(service);
            case IMAGE:
                return node.checkNode(service.binder);
            case TASK:
                return node.checkNode(taskMap);
            default:
                return node.checkNode(null);
        }
    }

    private Object getNodeTarget(Node node){
        switch (node.getType()){
            case TEXT:
            case TOUCH:
                return node.getNodeTarget(service);
            case IMAGE:
            case COLOR:
                return node.getNodeTarget(service.binder);
            case TASK:
                return node.getNodeTarget(taskMap);
            default:
                return node.getNodeTarget(null);
        }
    }

    private void addTaskProgress(Node node, boolean skip){
        if (node.getType() != NodeType.TASK){
            percent++;
            if (callback != null) callback.onProgress((percent) * 100 / allPercent);
        } else {
            if (skip){
                Integer integer = taskNodeMap.get(node);
                if (integer != null){
                    percent += integer;
                    if (callback != null) callback.onProgress(percent * 100 / allPercent);
                }
            }
        }
    }

    private void getAllTasks(Map<String, Task> taskMap, Task task){
        taskMap.put(task.getId(), task);
        for (Action action : task.getActions()) {
            if (action.isEnable()){
                for (Node target : action.getTargets()) {
                    if (target.getType() == NodeType.TASK) {
                        TaskNode.TaskInfo taskInfo = ((TaskNode) target).getValue();
                        if (!taskMap.containsKey(taskInfo.getId())){
                            List<Task> tasks = repository.getTasksById(taskInfo.getId());
                            if (tasks != null){
                                for (Task newTask : tasks) {
                                    getAllTasks(taskMap, newTask);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    private int getAllPercent(Task task){
        int percent = 0;
        for (Action action : task.getActions()) {
            if (action.isEnable()) {
                int cent = 0;
                for (Node target : action.getTargets()) {
                    if (target.getType() == NodeType.TASK){
                        Task newTask = taskMap.get(((TaskNode) target).getValue().getId());
                        if (newTask != null){
                            int taskCent = getAllPercent(newTask);
                            taskNodeMap.put(target, taskCent);
                            cent += taskCent;
                        }
                    } else {
                        cent++;
                    }
                }
                if (action.getActionMode() == ActionMode.LOOP){
                    cent *= action.getTimes();
                }
                percent += cent;
            }
        }
        return percent;
    }
}
