package top.bogey.auto_touch.room.data;

import android.graphics.Path;
import android.graphics.Rect;
import android.util.Log;
import android.view.accessibility.AccessibilityNodeInfo;
import android.widget.Toast;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.ActionMode;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.room.bean.SimpleTaskInfo;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.util.RunningCallback;

public class TaskRunnable implements Runnable{

    private final MainAccessibilityService service;
    private final Task task;
    private final TaskRepository repository;
    private final RunningCallback callback;

    private final Map<String, Task> taskMap = new HashMap<>();
    private final Map<Node, Integer> taskNodeMap = new HashMap<>();
    private final int allPercent;
    private int percent = 0;

    private boolean isRunning = true;

    public TaskRunnable(@NonNull MainAccessibilityService service, Task task, RunningCallback callback) {
        this.service = service;
        this.task = task;
        this.callback = callback;
        repository = new TaskRepository(service.getApplication());
        getAllTasks(taskMap, task);
        allPercent = getAllPercent(task);
    }

    public void stop() {
        isRunning = false;
    }

    public boolean isRunning(){
        return isRunning;
    }

    private void sleep(int time){
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void run() {
        doTask(task);
        if (callback != null) callback.onResult(isRunning());
    }

    private boolean doTask(@NonNull Task task){
        List<Action> actions = new ArrayList<>();
        for (Action action : task.getActions()) {
            if (action.isEnable()) actions.add(action);
        }

        boolean result = true;
        Action runAction = actions.remove(0);
        while (runAction != null && isRunning()){
            if (runAction.getActionMode() == ActionMode.CONDITION) {
                CheckResult checkResult = checkNode(runAction.getCondition(), true);
                if (checkResult.result){
                    result &= doAction(runAction, 0);
                    if (runAction.getTargets().size() > 1){
                        addNodePercent(runAction.getTargets().get(1), true);
                    }
                } else {
                    if (runAction.getTargets().size() > 1){
                        result &= doAction(runAction, 1);
                    } else {
                        result = false;
                    }
                    addNodePercent(runAction.getTargets().get(0), true);
                }
            } else if (runAction.getActionMode() == ActionMode.LOOP) {
                int successTimes = 0;
                int runTimes = 0;
                Node stop = runAction.getStop();
                while (runTimes < runAction.getTimes() && isRunning()){
                    for (int i = 0; i < runAction.getTargets().size(); i++) {
                        if (doAction(runAction, i)) successTimes++;
                    }
                    if (stop.getType() != NodeType.NULL){
                        if (stop.getType() == NodeType.NUMBER && successTimes >= stop.getNumber()){
                            break;
                        }
                        CheckResult checkResult = checkNode(stop, true);
                        if ((stop.getType() == NodeType.TEXT || stop.getType() == NodeType.IMAGE) && checkResult.result) break;
                    }
                    runTimes++;
                }
                // 有结束条件循环却一次都没成功，代表循环执行失败
                if (stop.getType() != NodeType.NULL && successTimes == 0){
                    result = false;
                }
            } else if (runAction.getActionMode() == ActionMode.PARALLEL) {
                CountDownLatch latch = new CountDownLatch(runAction.getStop().getNumber());
                Action finalRunAction = runAction;
                for (int i = 0; i < runAction.getTargets().size(); i++) {
                    int index = i;
                    service.taskService.submit(() -> {
                        if (doAction(finalRunAction, index)) latch.countDown();
                    });
                }
                try {
                    boolean await = latch.await(60, TimeUnit.SECONDS);
                    if (!await){
                        Toast.makeText(service, R.string.parallel_tips, Toast.LENGTH_LONG).show();
                        stop();
                        break;
                    }
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }

            if (actions.size() > 0){
                runAction = actions.remove(0);
            } else {
                runAction = null;
            }
        }

        // 线程关闭代表任务执行失败
        return result & isRunning();
    }

    private boolean doAction(Action action, int index){
        List<Node> targets = action.getTargets();
        if (targets.size() > index){
            Node target = targets.get(index);
            Log.d("TAG :" + percent, "执行动作目标: " + action.getTargetTitle(service, target));
            CheckResult checkResult = checkNode(target);
            boolean result = false;
            if (checkResult.result){
                result = true;
                switch (target.getType()) {
                    case DELAY:
                        sleep(target.getDelay());
                        break;
                    case TEXT:
                        AccessibilityNodeInfo nodeInfo = checkResult.nodeInfo;
                        if (action.getTime() <= 100)
                            nodeInfo.performAction(AccessibilityNodeInfo.ACTION_CLICK);
                        else
                            nodeInfo.performAction(AccessibilityNodeInfo.ACTION_LONG_CLICK);

                        sleep(action.getTime());
                        break;
                    case IMAGE:
                    case POS:
                        Path path = null;
                        if (target.getType() == NodeType.IMAGE){
                            path = getPath(Collections.singletonList(new Pos(checkResult.rect.centerX(), checkResult.rect.centerY())));
                        } else if (target.getType() == NodeType.POS){
                            path = getPath(target.getPoses());
                        }
                        if (path != null){
                            service.runGesture(path, action.getTime());
                        }

                        sleep(action.getTime());
                        break;
                    case KEY:
                        service.performGlobalAction(target.getKey() + 1);

                        sleep(action.getTime());
                        break;
                    case TASK:
                        if (checkResult.task != null){
                            result = doTask(checkResult.task);
                        }
                        break;
                }
            }
            addNodePercent(target, !checkResult.result);
            return result;
        }
        return false;
    }

    private CheckResult checkNode(Node node){
        return checkNode(node, false);
    }

    @NonNull
    private CheckResult checkNode(@NonNull Node node, boolean simpleResult){
        switch (node.getType()){
            case NULL:
                return new CheckResult(true);
            case NUMBER:
                int number = node.getNumber();
                return new CheckResult(number > 0, number);
            case DELAY:
                int delay = node.getDelay();
                return new CheckResult(delay > 0, delay);
            case TEXT:
                AccessibilityNodeInfo nodeInfo = service.getRootInActiveWindow();
                List<AccessibilityNodeInfo> nodes = searchNodes(nodeInfo, node.getText());
                if (simpleResult){
                    return new CheckResult(nodes != null && !nodes.isEmpty());
                } else {
                    return new CheckResult(nodes != null && !nodes.isEmpty(), searchClickableNode(nodes));
                }
            case IMAGE:
                if (service.binder != null){
                    Rect rect = service.binder.matchImage(node.getImage(), 90);
                    return new CheckResult(rect != null, rect);
                } else {
                    return new CheckResult(false);
                }
            case POS:
                List<Pos> poses = node.getPoses();
                return new CheckResult(poses != null && !poses.isEmpty());
            case KEY:
                int key = node.getKey();
                return new CheckResult(key >= 0, key);
            case TASK:
                Task task = taskMap.get(node.getTask().getId());
                if (task != null){
                    return new CheckResult(true, task);
                } else {
                    return new CheckResult(false);
                }
        }
        return new CheckResult(false);
    }

    private Path getPath(List<Pos> poses){
        if (poses != null && !poses.isEmpty()){
            Path path = new Path();
            Pos firstPos = poses.get(0);
            path.moveTo(firstPos.getX(), firstPos.getY());
            for (int i = 1; i < poses.size(); i++) {
                Pos pos = poses.get(i);
                path.lineTo(pos.getX(), pos.getY());
            }
            return path;
        }
        return null;
    }

    private List<AccessibilityNodeInfo> searchNodes(AccessibilityNodeInfo nodeInfo, String key){
        if (nodeInfo == null) return null;
        Pattern pattern = Pattern.compile("[\"|“](.*)[\"|”]");
        Matcher matcher = pattern.matcher(key);
        if (matcher.find()) {
            String realKey = matcher.group(1);
            if (realKey != null) {
                int i = realKey.indexOf("id/");
                if (i == 0){
                    return nodeInfo.findAccessibilityNodeInfosByViewId(task.getPkgName() + ":" + realKey);
                } else {
                    return nodeInfo.findAccessibilityNodeInfosByText(realKey);
                }
            }
        }
        ArrayList<AccessibilityNodeInfo> similarNodes = new ArrayList<>();
        searchNodes(similarNodes, nodeInfo, key);
        return similarNodes;
    }

    private void searchNodes(List<AccessibilityNodeInfo> similarNodes, AccessibilityNodeInfo nodeInfo, String key){
        if (nodeInfo == null) return;
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null){
                String text = String.valueOf(child.getText());
                String des = String.valueOf(child.getContentDescription());
                String id = String.valueOf(child.getViewIdResourceName());
                String lKey = key.toLowerCase();
                if (text.toLowerCase().contains(lKey) || des.toLowerCase().contains(lKey) || id.toLowerCase().contains(lKey)) {
                    similarNodes.add(child);
                } else {
                    searchNodes(similarNodes, child, key);
                }
            }
        }
    }

    private AccessibilityNodeInfo searchClickableNode(List<AccessibilityNodeInfo> nodes){
        if (nodes == null || nodes.isEmpty()) return null;
        for (AccessibilityNodeInfo node : nodes) {
            AccessibilityNodeInfo clickableNode = searchClickableNode(node);
            if (clickableNode != null) return clickableNode;
        }
        return null;
    }

    private AccessibilityNodeInfo searchClickableNode(AccessibilityNodeInfo nodeInfo){
        if (nodeInfo == null) return null;
        if (nodeInfo.isClickable()) return nodeInfo;
        return searchClickableNode(nodeInfo.getParent());
    }

    private void refreshRunningPercent(int percent){
        if (callback != null){
            callback.onProgress((int) (percent * 100.0 / allPercent));
        }
    }

    private synchronized void addNodePercent(Node node, boolean skip){
        if (node.getType() != NodeType.TASK){
            refreshRunningPercent(++percent);
        } else {
            if (skip){
                Integer integer = taskNodeMap.get(node);
                if (integer != null){
                    percent += integer;
                    refreshRunningPercent(percent);
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
                        SimpleTaskInfo taskInfo = target.getTask();
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
                        Task newTask = taskMap.get(target.getTask().getId());
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

    private static class CheckResult{
        public boolean result;
        public AccessibilityNodeInfo nodeInfo;
        public Rect rect;
        public Task task;
        public int number;

        public CheckResult(boolean result) {
            this.result = result;
        }

        public CheckResult(boolean result, AccessibilityNodeInfo nodeInfo) {
            this.result = result;
            this.nodeInfo = nodeInfo;
        }

        public CheckResult(boolean result, Rect rect) {
            this.result = result;
            this.rect = rect;
        }

        public CheckResult(boolean result, Task task) {
            this.result = result;
            this.task = task;
        }

        public CheckResult(boolean result, int number) {
            this.result = result;
            this.number = number;
        }
    }
}
