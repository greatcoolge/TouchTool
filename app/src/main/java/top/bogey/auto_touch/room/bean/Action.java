package top.bogey.auto_touch.room.bean;

import android.content.Context;

import java.util.List;

import top.bogey.auto_touch.R;

public class Action{
    private ActionMode actionMode = ActionMode.CONDITION;
    private boolean enable = true;

    private String title = "";
    private Node condition;
    private List<Node> targets;

    // 循环次数
    private int times = 1;
    // 循环结束条件
    private Node stop;

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
                    builder.append(context.getString(R.string.condition_title_2));
                    builder.append(getTargetTitle(context, targets.get(1)));
                }
                break;
            case LOOP:
                builder.append(context.getString(R.string.loop_title_1, times));
                for (int i = 0; i < targets.size(); i++) {
                    builder.append(getTargetTitle(context, targets.get(i)));
                    if (i == targets.size() - 1){
                        if (stop != null && stop.getType() != NodeType.NULL){
                            builder.append("\n");
                            builder.append(context.getString(R.string.loop_title_2, getConditionTitle(context, stop)));
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
                if (stop != null && stop.getType() == NodeType.NUMBER){
                    builder.append(stop.getNumber() == targets.size() ? context.getString(R.string.parallel_title_2) : context.getString(R.string.loop_title_2, getConditionTitle(context, stop)));
                }
                break;
        }
        return builder.toString();
    }

    private String getConditionTitle(Context context, Node node){
        if (node == null) return "";
        switch (node.getType()){
            case NULL:
                return context.getString(R.string.null_con);
            case NUMBER:
                return context.getString(R.string.number_con, node.getNumber());
            case TEXT:
                return context.getString(R.string.text_con, node.getText());
            case IMAGE:
                return context.getString(R.string.image_con);
        }
        return "";
    }

    public String getTargetTitle(Context context, Node node){
        if (node == null) return "";
        String touch = node.getTime() > 100 ? context.getString(R.string.long_touch) : context.getString(R.string.touch);
        switch (node.getType()){
            case DELAY:
                return context.getString(R.string.delay_target, node.getDelay());
            case TEXT:
                return context.getString(R.string.text_target, touch, node.getText());
            case IMAGE:
                return context.getString(R.string.image_target, touch);
            case POS:
                String slide = node.getPoses().size() > 1 ? context.getString(R.string.slide) : context.getString(R.string.touch);
                return context.getString(R.string.pos_target, slide);
            case KEY:
                String[] keys = context.getResources().getStringArray(R.array.keys);
                return context.getString(R.string.key_target, keys[node.getKey()]);
            case TASK:
                return context.getString(R.string.task_target, node.getTask().getTitle());
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

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Node getCondition() {
        return condition;
    }

    public void setCondition(Node condition) {
        this.condition = condition;
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

    public Node getStop() {
        return stop;
    }

    public void setStop(Node stop) {
        this.stop = stop;
    }
}
