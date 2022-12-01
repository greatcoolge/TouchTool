package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.os.Parcel;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;

public class TextAction extends Action {
    private String text;

    public TextAction() {
        super(ActionType.TEXT);
    }

    protected TextAction(Parcel in) {
        super(ActionType.TEXT);
        text = in.readString();
    }

    public AccessibilityNodeInfo searchClickableNode(List<AccessibilityNodeInfo> nodes) {
        if (nodes == null || nodes.isEmpty()) return null;
        for (AccessibilityNodeInfo node : nodes) {
            AccessibilityNodeInfo clickableNode = searchClickableNode(node);
            if (clickableNode != null) return clickableNode;
        }
        return null;
    }

    private AccessibilityNodeInfo searchClickableNode(AccessibilityNodeInfo nodeInfo) {
        if (nodeInfo == null) return null;
        if (nodeInfo.isClickable()) return nodeInfo;
        return searchClickableNode(nodeInfo.getParent());
    }

    public List<AccessibilityNodeInfo> searchNodes(AccessibilityNodeInfo nodeInfo) {
        if (nodeInfo == null) return null;
        if (text == null || text.isEmpty()) return null;
        Pattern pattern = Pattern.compile("[\"|“](.*)[\"|”]");
        Matcher matcher = pattern.matcher(text);
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
                } else if (realKey.indexOf("reg/") == 0) {
                    String[] split = realKey.split("/");
                    String regex = split[1];
                    nodeInfo = searchNode(nodeInfo, Pattern.compile(regex));
                    if (nodeInfo == null) return null;
                    return Collections.singletonList(nodeInfo);
                } else {
                    return nodeInfo.findAccessibilityNodeInfosByText(realKey);
                }
            }
        }
        ArrayList<AccessibilityNodeInfo> similarNodes = new ArrayList<>();
        searchNodes(similarNodes, nodeInfo, text);
        return similarNodes;
    }

    private void searchNodes(List<AccessibilityNodeInfo> similarNodes, AccessibilityNodeInfo nodeInfo, String key) {
        if (nodeInfo == null) return;
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null) {
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

    private AccessibilityNodeInfo searchNode(AccessibilityNodeInfo nodeInfo, int level) {
        int index = 0;
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null) {
                if (level == index) {
                    return child;
                } else {
                    index++;
                }
            }
        }
        return null;
    }

    private AccessibilityNodeInfo searchNode(AccessibilityNodeInfo nodeInfo, Pattern pattern) {
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null) {
                CharSequence text = child.getText();
                if (text != null && text.length() > 0) {
                    Matcher matcher = pattern.matcher(text);
                    if (matcher.find()) return child;
                }
                AccessibilityNodeInfo node = searchNode(child, pattern);
                if (node != null) return node;
            }
        }
        return null;
    }

    @Override
    public boolean isValid() {
        return text != null && !text.isEmpty();
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        List<AccessibilityNodeInfo> nodes = searchNodes(service.getRootInActiveWindow());
        return nodes != null && !nodes.isEmpty();
    }

    @Override
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        if (!super.doAction(task, service, runningInfo)) return false;

        AccessibilityNodeInfo root = service.getRootInActiveWindow();
        List<AccessibilityNodeInfo> nodes = searchNodes(root);
        AccessibilityNodeInfo nodeInfo = searchClickableNode(nodes);
        if (nodeInfo == null) return false;
        int time = getTimeArea().getRandomTime();
        if (time <= 500) {
            nodeInfo.performAction(AccessibilityNodeInfo.ACTION_CLICK);
        } else {
            nodeInfo.performAction(AccessibilityNodeInfo.ACTION_LONG_CLICK);
        }
        sleep(time);
        return true;
    }

    @Override
    public String getDescription(Context context, Task task, Behavior behavior) {
        if (context == null) return text;
        String touch = context.getString(timeArea.getMin() > 500 ? R.string.long_touch : R.string.touch);
        return context.getString(R.string.action_text, touch, text);
    }

    @Override
    public String getConditionContent(Context context, Task task, Behavior behavior) {
        return context.getString(R.string.condition_text, text);
    }

    @Override
    public String getConditionHint(Context context, Task task, Behavior behavior) {
        return context.getString(R.string.condition_text_tips);
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeString(text);
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;

        TextAction that = (TextAction) o;

        return text.equals(that.text);
    }

    @Override
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + text.hashCode();
        return result;
    }
}
