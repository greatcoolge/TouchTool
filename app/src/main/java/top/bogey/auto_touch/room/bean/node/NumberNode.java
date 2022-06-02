package top.bogey.auto_touch.room.bean.node;

public class NumberNode extends Node{
    public NumberNode(int number) {
        super(NodeType.NUMBER, number);
    }

    @Override
    public Integer getValue() {
        return (Integer) value;
    }

    @Override
    public boolean checkNode(Object obj) {
        return getValue() > 0;
    }

    @Override
    public Object getNodeTarget(Object obj) {
        if (checkNode(obj)) return getValue();
        return null;
    }
}
