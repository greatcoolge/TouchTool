package top.bogey.auto_touch.room.bean.node;

public class DelayNode extends Node {
    public DelayNode(TimeArea delay) {
        super(NodeType.DELAY, delay);
    }

    public String getTitle(){
        if (getValue().getMin() == getValue().getMax()){
            return String.valueOf(getValue().getMin());
        }
        return getValue().getRealMin() + "-" + getValue().getRealMax();
    }

    @Override
    public TimeArea getValue(){
        return (TimeArea) value;
    }

    @Override
    public boolean isValid() {
        return getValue().getRealMin() > 0;
    }

    @Override
    public boolean checkNode(Object obj) {
        return isValid();
    }

    @Override
    public Integer getNodeTarget(Object obj) {
        if (checkNode(obj)) return getValue().getRandomTime();
        return null;
    }
}
