package top.bogey.touch_tool.database.bean;

import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.Collections;
import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.ActionType;

public class Behavior implements Parcelable {
    // 行为模式
    private BehaviorMode behaviorMode;
    // 是否启用
    private boolean enable;

    // 行为包含的动作
    private List<Action> actions;

    // 行为执行的条件
    private Action condition;

    // 循环行为执行次数
    private int times;
    // 行为自定义标题
    private String title;

    public Behavior() {
        behaviorMode = BehaviorMode.CONDITION;
        enable = true;
    }

    public Behavior(Action action) {
        this();
        actions = Collections.singletonList(action);
    }

    protected Behavior(Parcel in) {
        behaviorMode = in.readParcelable(BehaviorMode.class.getClassLoader());
        enable = in.readByte() != 0;
        actions = in.createTypedArrayList(Action.CREATOR);
        condition = in.readParcelable(Action.class.getClassLoader());
        times = in.readInt();
        title = in.readString();
    }

    public static final Creator<Behavior> CREATOR = new Creator<Behavior>() {
        @Override
        public Behavior createFromParcel(Parcel in) {
            return new Behavior(in);
        }

        @Override
        public Behavior[] newArray(int size) {
            return new Behavior[size];
        }
    };

    public String getDefaultTitle(Context context) {
        if (actions == null || actions.isEmpty()) return "";
        StringBuilder builder = new StringBuilder();
        switch (behaviorMode) {
            case CONDITION:
                if (condition != null && condition.getType() != ActionType.NULL) {
                    builder.append(context.getString(R.string.condition_title_1, getConditionTitle(context, condition)));
                }
                builder.append(getTargetTitle(context, actions.get(0)));
                if (actions.size() > 1) {
                    builder.append("\n");
                    builder.append(context.getString(R.string.condition_title_2));
                    builder.append(getTargetTitle(context, actions.get(1)));
                }
                break;
            case LOOP:
                builder.append(context.getString(R.string.loop_title_1, times));
                for (int i = 0; i < actions.size(); i++) {
                    builder.append(getTargetTitle(context, actions.get(i)));
                    if (i == actions.size() - 1) {
                        if (condition != null && condition.getType() != ActionType.NULL) {
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
                for (Action target : actions) {
                    builder.append(getTargetTitle(context, target));
                    builder.append("\n");
                }
                if (condition != null) {
                    builder.append(condition.getDescription(context, true));
                }
                break;
        }
        return builder.toString();
    }

    private String getConditionTitle(Context context, Action action) {
        if (action == null) return "";
        switch (action.getType()) {
            case NUMBER:
                return context.getString(behaviorMode == BehaviorMode.LOOP ? R.string.condition_number_for_loop : R.string.condition_number_for_parallel, action.getDescription(context, true));
            case TEXT:
                return context.getString(R.string.condition_text, action.getDescription(context, true));
            case IMAGE:
                return context.getString(R.string.condition_image);
        }
        return "";
    }

    public String getTargetTitle(Context context, Action action) {
        if (action == null) return "";
        String touch = action.getTimeArea().getMax() > 100 ? context.getString(R.string.long_touch) : context.getString(R.string.touch);
        return action.getDescription(context, true);
    }

    public BehaviorMode getActionMode() {
        return behaviorMode;
    }

    public void setActionMode(BehaviorMode behaviorMode) {
        this.behaviorMode = behaviorMode;
    }

    public boolean isEnable() {
        return enable;
    }

    public void setEnable(boolean enable) {
        this.enable = enable;
    }

    public List<Action> getActions() {
        return actions;
    }

    public void setActions(List<Action> actions) {
        this.actions = actions;
    }

    public int getTimes() {
        return times;
    }

    public void setTimes(int times) {
        this.times = times;
    }

    public Action getCondition() {
        return condition;
    }

    public void setCondition(Action condition) {
        this.condition = condition;
    }

    public String getTitle(Context context) {
        if (title == null || title.isEmpty()) return getDefaultTitle(context);
        return title;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeParcelable(behaviorMode, flags);
        dest.writeByte((byte) (enable ? 1 : 0));
        dest.writeTypedList(actions);
        dest.writeParcelable(condition, flags);
        dest.writeInt(times);
        dest.writeString(title);
    }
}
