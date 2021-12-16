package top.bogey.auto_touch.room.data;

import android.graphics.Path;
import android.graphics.Rect;
import android.util.Log;
import android.view.accessibility.AccessibilityNodeInfo;
import android.widget.Toast;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
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
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.util.CompleteCallback;

public class TaskRunnable implements Runnable{

    private final MainAccessibilityService service;
    private final Task task;
    private final TaskRepository repository;
    private final CompleteCallback callback;

    private boolean isRunning = true;

    public TaskRunnable(@NonNull MainAccessibilityService service, Task task, CompleteCallback callback) {
        this.service = service;
        this.task = task;
        repository = new TaskRepository(service.getApplication());
        this.callback = callback;
    }

    public void stop() {
        isRunning = false;
    }

    public boolean isRunning(){
        return isRunning;
    }

    @Override
    public void run() {
        doTask(task);
        if (callback != null) callback.onComplete();
    }

    private void doTask(@NonNull Task task){
        Log.d("TAG", "执行任务: " + task.getTitle());
        List<Action> actions = new ArrayList<>();
        for (Action action : task.getActions()) {
            if (action.isEnable()) actions.add(action);
        }

        Action runAction = actions.remove(0);
        while (runAction != null && isRunning()){
            Log.d("TAG", "执行动作: " + runAction.getDefaultTitle(service));
            if (runAction.getActionMode() == ActionMode.CONDITION) {
                CheckResult checkResult = checkNode(runAction.getCondition(), true);
                if (checkResult.result){
                    doAction(runAction, 0);
                } else {
                    if (runAction.getTargets().size() > 1){
                        doAction(runAction, 1);
                    }
                }
            } else if (runAction.getActionMode() == ActionMode.LOOP) {
                int successTimes = 0;
                int runTimes = 0;
                while (runTimes < runAction.getTimes() && isRunning()){
                    for (int i = 0; i < runAction.getTargets().size(); i++) {
                        if (doAction(runAction, i)) successTimes++;
                    }
                    Node stop = runAction.getStop();
                    if (stop.getType() != NodeType.NULL){
                        if (stop.getType() == NodeType.NUMBER && successTimes >= stop.getNumber()){
                            break;
                        }
                        CheckResult checkResult = checkNode(stop, true);
                        if ((stop.getType() == NodeType.TEXT || stop.getType() == NodeType.IMAGE) && checkResult.result) break;
                    }
                    runTimes++;
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
    }

    private boolean doAction(Action action, int index){
        List<Node> targets = action.getTargets();
        if (targets.size() > index){
            Node target = targets.get(index);
            Log.d("TAG", "执行动作目标: " + action.getTargetTitle(service, target));
            CheckResult result = checkNode(target);
            if (result.result){
                switch (target.getType()) {
                    case DELAY:
                        try {
                            Thread.sleep(target.getDelay());
                            return true;
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                            return false;
                        }
                    case TEXT:
                        AccessibilityNodeInfo nodeInfo = result.nodeInfo;
                        if (action.getTime() <= 100)
                            nodeInfo.performAction(AccessibilityNodeInfo.ACTION_CLICK);
                        else
                            nodeInfo.performAction(AccessibilityNodeInfo.ACTION_LONG_CLICK);
                        try {
                            Thread.sleep(action.getTime());
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                        return true;
                    case IMAGE:
                    case POS:
                        Path path = null;
                        if (target.getType() == NodeType.IMAGE){
                            path = getPath(Collections.singletonList(new Pos(result.rect.centerX(), result.rect.centerY())));
                        } else if (target.getType() == NodeType.POS){
                            path = getPath(target.getPoses());
                        }
                        if (path != null){
                            service.runGesture(path, action.getTime());
                        }

                        try {
                            Thread.sleep(action.getTime());
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                        return true;
                    case KEY:
                        service.performGlobalAction(target.getKey() + 1);
                        return true;
                    case TASK:
                        if (result.task != null){
                            doTask(result.task);
                        }
                        return true;
                }
            }
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
                List<Task> list = repository.getTasksById(node.getTask().getId());
                if (list != null && !list.isEmpty()){
                    return new CheckResult(true, list.get(0));
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
