package top.bogey.touch_tool.room.bean.node;

public class NumberNode extends Node{
    public NumberNode(int number) {
        super(NodeType.NUMBER, number);
    }

    @Override
    public Integer getValue() {
        return (Integer) value;
    }

    @Override
    public boolean isValid() {
        return getValue() > 0;
    }

    @Override
    public boolean checkNode(Object obj) {
        return isValid();
    }

    @Override
    public Object getNodeTarget(Object obj) {
        if (checkNode(obj)) return getValue();
        return null;
    }

    @Override
    public Integer cloneValue() {
        return getValue();
    }
}
