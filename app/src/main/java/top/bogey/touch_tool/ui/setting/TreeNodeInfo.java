package top.bogey.touch_tool.ui.setting;

public class TreeNodeInfo {
    private final String key;
    private final int value;
    private final int success;

    public TreeNodeInfo(String key, int value, int success) {
        this.key = key;
        this.value = value;
        this.success = success;
    }

    public String getKey() {
        return key;
    }

    public int getValue() {
        return value;
    }

    public int getSuccess() {
        return success;
    }
}
