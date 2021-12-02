package top.bogey.auto_touch.room.data;

import android.accessibilityservice.GestureDescription;
import android.graphics.Path;
import android.graphics.Rect;
import android.util.Log;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.ActionMode;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.util.CompleteCallback;

public class TaskRunnable implements Runnable{

    private final MainAccessibilityService service;
    private final Task task;
    private final TaskRepository repository;
    private final CompleteCallback callback;

    public TaskRunnable(@NonNull MainAccessibilityService service, Task task, CompleteCallback callback) {
        this.service = service;
        this.task = task;
        repository = new TaskRepository(service.getApplication());
        this.callback = callback;
    }

    public void stop() {
        Thread.currentThread().interrupt();
    }

    public boolean isRunning(){
        return !Thread.currentThread().isInterrupted();
    }

    @Override
    public void run() {
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(task.id);
        doConfig(task, ids);
        if (callback != null) callback.onComplete();
    }

    private void doConfig(@NonNull Task task, List<Integer> ids){
        List<Action> actions = new ArrayList<>();
        for (Action action : task.actions) {
            if (action.enable) actions.add(action);
        }

        Action runAction = actions.remove(0);
        while (runAction != null && isRunning()){

            try {
                Thread.sleep(runAction.delay);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            int runTimes = 0;
            List<Integer> cacheIds = new ArrayList<>();
            while (runTimes < runAction.times && isRunning()){
                if (checkNodes(runAction.keys)){
                    CheckResult checkResult = checkNode(runAction.target);
                    if (checkResult != null && checkResult.result){
                        Rect rect = null;
                        switch (runAction.target.type){
                            case WORD:
                                AccessibilityNodeInfo nodeInfo = checkResult.nodeInfo;
                                if (runAction.time <= 100)
                                    nodeInfo.performAction(AccessibilityNodeInfo.ACTION_CLICK);
                                else
                                    nodeInfo.performAction(AccessibilityNodeInfo.ACTION_LONG_CLICK);
                                break;
                            case IMAGE:
                                rect = checkResult.rect;
                            case POS:
                                List<Pos> posList = new ArrayList<>();
                                if (rect != null){
                                    posList.add(new Pos(rect.centerX(), rect.centerY()));
                                } else {
                                    posList.addAll(runAction.target.getPoses());
                                }
                                Path path = getPath(posList);
                                if (path != null){
                                    service.dispatchGesture(new GestureDescription.Builder().addStroke(
                                            new GestureDescription.StrokeDescription(path, 0, runAction.time)
                                    ).build(), null, null);
                                }
                                break;
                            case TASK:
                                switch (runAction.actionMode){
                                    case KEY:
                                        service.performGlobalAction(runAction.target.getTask().id);
                                        break;
                                    case TASK:
                                        Task newTask = checkResult.task;
                                        if (newTask != null){
                                            if (!ids.contains(newTask.id) && newTask.taskStatus != TaskStatus.AUTO){
                                                cacheIds.clear();
                                                cacheIds.addAll(ids);
                                                cacheIds.add(newTask.id);
                                                doConfig(newTask, cacheIds);
                                            }
                                        }
                                        break;
                                }
                                break;
                        }
                        Log.d("Action", "run: " + runAction.getTitle(service.getApplicationContext()));
                        CheckResult stopResult = checkNode(runAction.stop, true);
                        if (stopResult != null && stopResult.result){
                            stop();
                            return;
                        }
                    }
                }

                try {
                    Thread.sleep(runAction.interval);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }

                runTimes++;
            }

            if (runAction.actionMode == ActionMode.TASK){
                ids.clear();
                ids.addAll(cacheIds);
            }

            if (actions.size() > 0) {
                runAction = actions.remove(0);
            } else {
                runAction = null;
            }
        }
    }

    private CheckResult checkNode(Node node){
        return checkNode(node, false);
    }

    @Nullable
    private CheckResult checkNode(@NonNull Node node, boolean simpleResult){
        switch (node.type){
            case NULL:
                return new CheckResult(false);
            case BOOL:
                return new CheckResult(true);
            case POS:
                List<Pos> poses = node.getPoses();
                return new CheckResult(poses != null && !poses.isEmpty());
            case WORD:
                AccessibilityNodeInfo nodeInfo = service.getRootInActiveWindow();
                List<AccessibilityNodeInfo> nodes = searchNodes(nodeInfo, node.getWord());
                if (simpleResult){
                    return new CheckResult(nodes != null);
                } else {
                    return new CheckResult(nodes != null, searchClickableNode(nodes));
                }
            case IMAGE:
                if (service.binder != null){
                    Rect rect = service.binder.matchImage(node.getImage(), 90);
                    return new CheckResult(rect != null, rect);
                } else {
                    return new CheckResult(false);
                }
            case TASK:
                List<Task> list = repository.getTasksById(node.getTask().id);
                if (list != null && !list.isEmpty()){
                    return new CheckResult(true, list.get(0));
                } else {
                    // 可能是按键任务，总是返回true
                    return new CheckResult(true);
                }
        }
        return null;
    }

    private boolean checkNodes(List<Node> nodes){
        if (nodes == null) return true;
        for (Node node : nodes) {
            CheckResult checkResult = checkNode(node, true);
            if (checkResult == null || !checkResult.result){
                return false;
            }
        }
        return true;
    }

    private Path getPath(List<Pos> poses){
        if (poses != null && !poses.isEmpty()){
            Path path = new Path();
            Pos firstPos = poses.get(0);
            path.moveTo(firstPos.x, firstPos.y);
            for (int i = 1; i < poses.size(); i++) {
                Pos pos = poses.get(i);
                path.lineTo(pos.x, pos.y);
            }
            return path;
        }
        return null;
    }

    private List<AccessibilityNodeInfo> searchNodes(AccessibilityNodeInfo nodeInfo, String key){
        if (nodeInfo == null) return null;
        ArrayList<AccessibilityNodeInfo> identicalNodes = new ArrayList<>();
        ArrayList<AccessibilityNodeInfo> similarNodes = new ArrayList<>();
        searchNodes(identicalNodes, similarNodes, nodeInfo, key);
        identicalNodes.addAll(similarNodes);
        return identicalNodes;
    }

    private void searchNodes(List<AccessibilityNodeInfo> identicalNodes, List<AccessibilityNodeInfo> similarNodes, AccessibilityNodeInfo nodeInfo, String key){
        if (nodeInfo == null) return;
        Pattern pattern = Pattern.compile("[\"|“](.*)[\"|”]");
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null){
                String text = String.valueOf(child.getText());
                String des = String.valueOf(child.getContentDescription());
                String id = String.valueOf(child.getViewIdResourceName());
                Matcher matcher = pattern.matcher(key);
                boolean flag = true;
                // 需要完全匹配的
                if (matcher.find() && matcher.group(1) != null) {
                    String realKey = matcher.group(1);
                    if (text.equals(realKey) || des.equals(realKey) || id.equals(realKey)) {
                        identicalNodes.add(child);
                        flag = false;
                    }
                } else {
                    //完全匹配
                    if (text.equals(key) || des.equals(key) || id.equals(key)) {
                        identicalNodes.add(child);
                        flag = false;
                    } else {
                        String lKey = key.toLowerCase();
                        if (text.toLowerCase().contains(lKey) || des.toLowerCase().contains(lKey) || id.toLowerCase().contains(lKey)) {
                            similarNodes.add(child);
                            flag = false;
                        }
                    }
                }
                if (flag) {
                    searchNodes(identicalNodes, similarNodes, child, key);
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

    private static class CheckResult{
        public boolean result;
        public AccessibilityNodeInfo nodeInfo;
        public Rect rect;
        public Task task;

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
    }
}
