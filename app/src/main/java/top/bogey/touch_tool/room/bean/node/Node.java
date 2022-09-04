package top.bogey.touch_tool.room.bean.node;

import androidx.annotation.NonNull;

public abstract class Node implements Cloneable {
    protected final NodeType type;
    protected Object value;
    protected final TimeArea timeArea = new TimeArea(100, 100);

    protected Node(NodeType type){
        this.type = type;
    }

    public Node(NodeType type, Object value) {
        this.type = type;
        this.value = value;
    }

    public Object getValue(){
        return value;
    }

    public void setValue(Object value){
        this.value = value;
    }

    public TimeArea getTimeArea() {
        return timeArea;
    }

    public NodeType getType() {
        return type;
    }

    public abstract boolean isValid();

    public abstract boolean checkNode(Object obj);

    public abstract Object getNodeTarget(Object obj);

    @NonNull
    @Override
    public Node clone() {
        Node node = this;
        try {
            Node newNode = getClass().cast(super.clone());
            if (newNode != null) node = newNode;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return node;
    }
}
