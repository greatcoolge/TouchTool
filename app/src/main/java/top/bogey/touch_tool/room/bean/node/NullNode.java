package top.bogey.touch_tool.room.bean.node;

public class NullNode extends Node {
    public NullNode() {
        super(NodeType.NULL);
    }

    @Override
    public Object getValue() {
        return null;
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
        return null;
    }

    @Override
    public Object cloneValue() {
        return null;
    }
}
