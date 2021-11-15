package top.bogey.auto_touch.room.bean;

import android.content.Context;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;

public class Action implements Cloneable {
    public ActionMode actionMode = ActionMode.WORD;
    public boolean enable = true;

    public List<Node> keys;
    public Node target;
    public Node stop;

    // 开始执行的延时
    public int delay = 100;
    // 执行次数
    public int times = 1;
    // 执行间隔
    public int interval = 100;
    // 执行时长
    public int time = 100;

    public boolean checkSafeTime(){
        return delay + Math.max(1, interval) * times + Math.max(1, time) * times <= 60 * 1000;
    }

    @NonNull
    @Override
    public Action clone() {
        try {
            Action action = (Action) super.clone();
            if (keys != null){
                action.keys = new ArrayList<>(keys);
            }
            return action;
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return new Action();
        }
    }

    public String getTitle(Context context){
        switch (actionMode) {
            case WORD:
                return target.getWord();
            case KEY:
                String[] strings = context.getResources().getStringArray(R.array.keys);
                return strings[Integer.parseInt(target.getWord()) - 1];
            case GESTURE:
                StringBuilder builder = new StringBuilder();
                for (Pos pos : target.getPoses()) {
                    builder.append(pos.toString());
                }
                return builder.toString();
            case TASK:
                return target.getTask().title;
        }
        return "";
    }
}
