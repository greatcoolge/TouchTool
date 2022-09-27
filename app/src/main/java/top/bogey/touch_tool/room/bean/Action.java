package top.bogey.touch_tool.room.bean;

import android.content.Context;

import java.util.List;
import java.util.UUID;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.room.bean.node.DelayNode;
import top.bogey.touch_tool.room.bean.node.Node;
import top.bogey.touch_tool.room.bean.node.NodeType;
import top.bogey.touch_tool.room.bean.node.NumberNode;
import top.bogey.touch_tool.room.bean.node.TaskNode;
import top.bogey.touch_tool.room.bean.node.TextNode;
import top.bogey.touch_tool.room.bean.node.TouchNode;

public class Action {
    private ActionMode actionMode = ActionMode.CONDITION;
    private boolean enable = true;

    private List<Node> targets;
    private int times = 1;
    private Node condition;

    private String title = "";

    public String getDefaultTitle(Context context){
        if (targets == null || targets.isEmpty()) return "";
        StringBuilder builder = new StringBuilder();
        switch (actionMode) {
            case CONDITION:
                if (condition != null && condition.getType() != NodeType.NULL){
                    builder.append(context.getString(R.string.condition_title_1, getConditionTitle(context, condition)));
                }
                builder.append(getTargetTitle(context, targets.get(0)));
                if (targets.size() > 1){
                    builder.append("\n");
                    builder.append(context.getString(R.string.condition_title_2));
                    builder.append(getTargetTitle(context, targets.get(1)));
                }
                break;
            case LOOP:
                builder.append(context.getString(R.string.loop_title_1, times));
                for (int i = 0; i < targets.size(); i++) {
                    builder.append(getTargetTitle(context, targets.get(i)));
                    if (i == targets.size() - 1){
                        if (condition != null && condition.getType() != NodeType.NULL){
                            builder.append("\n");
                            builder.append(context.getString(R.string.loop_title_2, getConditionTitle(context, condition)));
                        }
                    } else {
                        builder.append("\n");
                    }
                }
                break;
            case PARALLEL:
                builder.append(context.getString(R.string.parallel_title_1));
                for (Node target : targets) {
                    builder.append(getTargetTitle(context, target));
                    builder.append("\n");
                }
                if (condition != null && condition.getType() == NodeType.NUMBER){
                    builder.append(((NumberNode) condition).getValue() == targets.size() ? context.getString(R.string.parallel_title_2) : context.getString(R.string.loop_title_2, getConditionTitle(context, condition)));
                }
                break;
        }
        return builder.toString();
    }

    private String getConditionTitle(Context context, Node node){
        if (node == null) return "";
        switch (node.getType()){
            case NUMBER:
                return context.getString(actionMode == ActionMode.LOOP ? R.string.number_con_loop : R.string.number_con_parallel, ((NumberNode) node).getValue());
            case TEXT:
                return context.getString(R.string.text_con, ((TextNode) node).getValue());
            case IMAGE:
                return context.getString(R.string.image_con);
        }
        return "";
    }

    public String getTargetTitle(Context context, Node node){
        if (node == null) return "";
        String touch = node.getTimeArea().getMax() > 100 ? context.getString(R.string.long_touch) : context.getString(R.string.touch);
        switch (node.getType()){
            case DELAY:
                return context.getString(R.string.delay_target, ((DelayNode) node).getTitle());
            case TEXT:
                return context.getString(R.string.text_target, touch, ((TextNode) node).getValue());
            case IMAGE:
                return context.getString(R.string.image_target, touch);
            case TOUCH:
                String slide = ((TouchNode) node).getPoints().size() > 1 ? context.getString(R.string.slide) : context.getString(R.string.touch);
                return context.getString(R.string.pos_target, slide);
            case COLOR:
                return context.getString(R.string.color_target);
            case KEY:
                String[] keys = context.getResources().getStringArray(R.array.keys);
                String[] ids = context.getResources().getStringArray(R.array.key_ids);
                String str = String.valueOf(node.getValue());
                int index = 0;
                for (String id : ids) {
                    if (id.equals(str)) break;
                    index++;
                }
                return context.getString(R.string.key_target, keys[index]);
            case TASK:
                return context.getString(R.string.task_target, ((TaskNode) node).getValue().getTitle());
        }
        return "";
    }

    public ActionMode getActionMode() {
        return actionMode;
    }

    public void setActionMode(ActionMode actionMode) {
        this.actionMode = actionMode;
    }

    public boolean isEnable() {
        return enable;
    }

    public void setEnable(boolean enable) {
        this.enable = enable;
    }

    public List<Node> getTargets() {
        return targets;
    }

    public void setTargets(List<Node> targets) {
        this.targets = targets;
    }

    public int getTimes() {
        return times;
    }

    public void setTimes(int times) {
        this.times = times;
    }

    public Node getCondition() {
        return condition;
    }

    public void setCondition(Node condition) {
        this.condition = condition;
    }

    public String getTitle(Context context) {
        if (title.isEmpty()) return getDefaultTitle(context);
        return title;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }
}
