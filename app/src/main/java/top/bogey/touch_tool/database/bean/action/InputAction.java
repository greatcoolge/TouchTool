package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.os.Bundle;
import android.os.Parcel;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;

import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;

public class InputAction extends Action {
    private String id;
    private String text;

    public InputAction() {
        super(ActionType.INPUT);
    }

    protected InputAction(Parcel in) {
        super(ActionType.INPUT);
        id = in.readString();
        text = in.readString();
    }

    public AccessibilityNodeInfo searchInputBox(List<AccessibilityNodeInfo> nodes) {
        if (nodes == null || nodes.isEmpty()) return null;
        for (AccessibilityNodeInfo node : nodes) {
            AccessibilityNodeInfo clickableNode = searchInputBox(node);
            if (clickableNode != null) return clickableNode;
        }
        return null;
    }

    private AccessibilityNodeInfo searchInputBox(AccessibilityNodeInfo nodeInfo) {
        if (nodeInfo == null) return null;
        if (nodeInfo.isEditable()) return nodeInfo;
        return searchInputBox(nodeInfo.getParent());
    }

    public List<AccessibilityNodeInfo> searchNodes(AccessibilityNodeInfo nodeInfo) {
        if (nodeInfo == null) return null;
        if (id == null || id.isEmpty()) return null;
        Pattern pattern = Pattern.compile("[\"|“](.*)[\"|”]");
        Matcher matcher = pattern.matcher(id);
        if (matcher.find()) {
            String realKey = matcher.group(1);
            if (realKey != null) {
                if (realKey.indexOf("id/") == 0) {
                    return nodeInfo.findAccessibilityNodeInfosByViewId(nodeInfo.getPackageName() + ":" + realKey);
                } else if (realKey.indexOf("lv/") == 0) {
                    String[] split = realKey.split("/");
                    String[] levels = split[1].split(",");
                    for (String level : levels) {
                        nodeInfo = searchNode(nodeInfo, Integer.parseInt(level));
                        if (nodeInfo == null) return null;
                    }
                    return Collections.singletonList(nodeInfo);
                }
            }
        }
        return null;
    }

    private AccessibilityNodeInfo searchNode(AccessibilityNodeInfo nodeInfo, int level) {
        if (nodeInfo.getChildCount() > level) {
            return nodeInfo.getChild(level);
        }
        return null;
    }

    @Override
    public boolean isValid() {
        return id != null && !id.isEmpty() && text != null && !text.isEmpty();
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        return false;
    }

    @Override
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        if (!super.doAction(task, service, runningInfo)) return false;

        AccessibilityNodeInfo root = service.getRootInActiveWindow();
        List<AccessibilityNodeInfo> nodes = searchNodes(root);
        AccessibilityNodeInfo nodeInfo = searchInputBox(nodes);
        if (nodeInfo == null) return false;

        nodeInfo.performAction(AccessibilityNodeInfo.ACTION_FOCUS);
        Bundle bundle = new Bundle();
        bundle.putCharSequence(AccessibilityNodeInfo.ACTION_ARGUMENT_SET_TEXT_CHARSEQUENCE, text);
        nodeInfo.performAction(AccessibilityNodeInfo.ACTION_SET_TEXT, bundle);
        sleep(getTimeArea().getRandomTime());
        return true;
    }

    @Override
    public String getDescription(Context context, Task task, Behavior behavior) {
        if (context == null) return text;
        return context.getString(R.string.action_input, text);
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeString(id);
        dest.writeString(text);
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
}
