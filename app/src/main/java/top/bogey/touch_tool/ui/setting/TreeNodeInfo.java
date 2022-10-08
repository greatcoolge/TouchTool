package top.bogey.touch_tool.ui.setting;

public class TreeNodeInfo {
    private final String key;
    private final int value;

    public TreeNodeInfo(String key, int value) {
        this.key = key;
        this.value = value;
    }

    public String getKey() {
        return key;
    }

    public int getValue() {
        return value;
    }
}
