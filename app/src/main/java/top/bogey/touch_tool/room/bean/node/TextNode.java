package top.bogey.touch_tool.room.bean.node;

import android.view.accessibility.AccessibilityNodeInfo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;

public class TextNode extends Node{
    public TextNode(String value) {
        super(NodeType.TEXT, value);
    }

    @Override
    public String getValue() {
        return (String) value;
    }

    @Override
    public boolean isValid() {
        String value = getValue();
        return value != null && !value.isEmpty();
    }

    @Override
    public boolean checkNode(Object obj) {
        MainAccessibilityService service = (MainAccessibilityService) obj;
        AccessibilityNodeInfo root = service.getRootInActiveWindow();
        List<AccessibilityNodeInfo> nodes = searchNodes(root);
        return nodes != null && !nodes.isEmpty();
    }

    @Override
    public Object getNodeTarget(Object obj) {
        MainAccessibilityService service = (MainAccessibilityService) obj;
        AccessibilityNodeInfo root = service.getRootInActiveWindow();
        List<AccessibilityNodeInfo> nodes = searchNodes(root);
        return searchClickableNode(nodes);
    }

    @Override
    public String cloneValue() {
        return getValue();
    }

    public AccessibilityNodeInfo searchClickableNode(List<AccessibilityNodeInfo> nodes){
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

    public List<AccessibilityNodeInfo> searchNodes(AccessibilityNodeInfo nodeInfo){
        if (nodeInfo == null) return null;
        String key = getValue();
        if (key == null || key.isEmpty()) return null;
        Pattern pattern = Pattern.compile("[\"|“](.*)[\"|”]");
        Matcher matcher = pattern.matcher(key);
        if (matcher.find()) {
            String realKey = matcher.group(1);
            if (realKey != null) {
                if (realKey.indexOf("id/") == 0){
                    return nodeInfo.findAccessibilityNodeInfosByViewId(nodeInfo.getPackageName() + ":" + realKey);
                } else if (realKey.indexOf("lv/") == 0){
                    String[] split = realKey.split("/");
                    String[] levels = split[1].split(",");
                    for (String level : levels) {
                        nodeInfo = searchNode(nodeInfo, Integer.parseInt(level));
                        if (nodeInfo == null) return null;
                    }
                    return Collections.singletonList(nodeInfo);
                } else if(realKey.indexOf("reg/") == 0){
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

    private AccessibilityNodeInfo searchNode(AccessibilityNodeInfo nodeInfo, int level){
        int index = 0;
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null){
                if (level == index){
                    return child;
                } else {
                    index++;
                }
            }
        }
        return null;
    }

    private AccessibilityNodeInfo searchNode(AccessibilityNodeInfo nodeInfo, Pattern pattern){
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null){
                CharSequence text = child.getText();
                if (text != null && text.length() > 0){
                    Matcher matcher = pattern.matcher(text);
                    if (matcher.find()) return child;
                }
                AccessibilityNodeInfo node = searchNode(child, pattern);
                if (node != null) return node;
            }
        }
        return null;
    }
}
