package top.bogey.auto_touch.room.bean;

import java.util.List;

public class Action {
    public Mode mode = Mode.WORD;
    public boolean enable = true;

    public List<Node> keys;
    public Node target;
    public Node stop;
}
