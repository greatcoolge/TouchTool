package top.bogey.auto_touch.room.data;

import android.accessibilityservice.GestureDescription;
import android.graphics.Path;
import android.util.Log;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;

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

    public boolean isStopped(){
        return !Thread.currentThread().isInterrupted();
    }

    @Override
    public void run() {
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(task.id);
        doConfig(task, ids);
        if (callback != null) callback.onComplete();
    }

    private void doConfig(Task config, List<Integer> ids){
        List<Action> actions = new ArrayList<>();
        for (Action action : config.actions) {
            if (action.enable) actions.add(action);
        }

        Action runAction = actions.remove(0);
        while (runAction != null && isStopped()){
            try {
                Thread.sleep(runAction.delay);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            int runTimes = 0;
            List<Integer> cacheIds = new ArrayList<>();
            while (runTimes < runAction.times && isStopped()){
                AccessibilityNodeInfo nodeInfo = service.getRootInActiveWindow();
                if (checkWindow(nodeInfo, runAction)){
                    AccessibilityNodeInfo target = checkTarget(nodeInfo, runAction);
                    if (target != null){
                        switch (runAction.actionMode){
                            case WORD:
                                if (runAction.time <= 100) target.performAction(AccessibilityNodeInfo.ACTION_CLICK);
                                else target.performAction(AccessibilityNodeInfo.ACTION_LONG_CLICK);
                                break;
                            case KEY:
                                service.performGlobalAction(Integer.parseInt(runAction.target.getWord()));
                                break;
                            case GESTURE:
                                Path path = getPath(runAction.target);
                                if (path != null){
                                    service.dispatchGesture(
                                            new GestureDescription.Builder().addStroke(
                                                    new GestureDescription.StrokeDescription(path, 0, runAction.time)
                                            ).build(), null, null);
                                    try {
                                        Thread.sleep(runAction.time);
                                    } catch (InterruptedException e) {
                                        e.printStackTrace();
                                    }
                                }
                                break;
                            case TASK:
                                List<Task> list = repository.getTasksById(runAction.target.getTask().id);
                                for (Task cfg : list) {
                                    if (!ids.contains(cfg.id) && cfg.taskStatus != TaskStatus.AUTO){
                                        cacheIds.clear();
                                        cacheIds.addAll(ids);
                                        cacheIds.add(cfg.id);
                                        doConfig(cfg, cacheIds);
                                    }
                                }
                                break;
                        }
                        Log.d("Config", "run: " + runAction.getTitle(service.getApplicationContext()));
//                        if (runAction.isAutoStop()) break;
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

    private AccessibilityNodeInfo checkTarget(AccessibilityNodeInfo nodeInfo, Action action){
        if (action.actionMode != ActionMode.WORD) return nodeInfo;
        List<AccessibilityNodeInfo> nodes = searchNodes(nodeInfo, action.target.getWord());
        return searchClickableNode(nodes);
    }

    private boolean checkWindow(AccessibilityNodeInfo nodeInfo, Action action){
        // 校验节点
        if (nodeInfo.getChildCount() == 0) return false;

        List<Node> keys = action.keys;
        // 没有关键点限制
        if (keys == null || keys.isEmpty()) return true;

        // 校验关键节点
        for (Node node : keys) {
            List<AccessibilityNodeInfo> nodes = searchNodes(nodeInfo, node.getWord());
            if (nodes == null || nodes.isEmpty()) return false;
        }
        return true;
    }

    private Path getPath(Node node){
        List<Pos> poses = node.getPoses();
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
}
