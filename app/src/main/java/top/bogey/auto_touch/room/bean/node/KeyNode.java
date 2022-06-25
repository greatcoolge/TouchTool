package top.bogey.auto_touch.room.bean.node;

public class KeyNode extends Node{
    public KeyNode(Integer value) {
        super(NodeType.KEY, value);
    }

    @Override
    public Integer getValue() {
        return (Integer) value;
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public boolean checkNode(Object obj) {
        return true;
    }

    @Override
    public Object getNodeTarget(Object obj) {
        return getValue();
    }
}
