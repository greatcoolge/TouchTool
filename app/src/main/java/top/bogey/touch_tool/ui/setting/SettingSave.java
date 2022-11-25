package top.bogey.touch_tool.ui.setting;

import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.graphics.Point;
import android.view.View;

import androidx.appcompat.app.AppCompatDelegate;

import com.google.android.material.color.DynamicColors;
import com.google.android.material.color.DynamicColorsOptions;
import com.tencent.mmkv.MMKV;

import java.util.Comparator;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskConfig;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.ui.play.OverseeMode;
import top.bogey.touch_tool.ui.play.PlayFloatView;
import top.bogey.touch_tool.ui.task.SortType;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

public class SettingSave {
    private final static String FIRST_RUN = "FIRST_RUN";
    private static final String SERVICE_ENABLED = "SERVICE_ENABLED";
    private final static String SORT_TYPE = "SORT_TYPE";

    private final static String KEEP_ALIVE = "KEEP_ALIVE";
    private final static String ACTION_TOUCH_OFFSET = "ACTION_TOUCH_OFFSET";
    private final static String ACTION_RECORD_DELAY = "ACTION_RECORD_DELAY";
    private final static String EVENT_TIMEOUT = "EVENT_TIMEOUT";

    private final static String RUNNING_OVERSEE_MODE = "RUNNING_OVERSEE_MODE";
    private final static String RUNNING_LOG = "RUNNING_LOG";
    private final static String RUNNING_TASK_INFO = "RUNNING_TASK_INFO";

    private final static String NIGHT_MODE = "NIGHT_MODE";
    private final static String DYNAMIC_COLOR = "DYNAMIC_COLOR";

    private static SettingSave settingSave;

    private boolean isAppliedDynamicColor = false;
    private boolean isDynamicColor = true;
    private final DynamicColorsOptions options = new DynamicColorsOptions.Builder().setPrecondition((activity, theme) -> isDynamicColor).build();

    private final MMKV settingMMKV;

    public static SettingSave getInstance() {
        if (settingSave == null) settingSave = new SettingSave();
        return settingSave;
    }

    public SettingSave() {
        settingMMKV = MMKV.defaultMMKV();
    }

    public void init(Context context) {
        setKeepAlive(context, isKeepAlive());
        setRunningOverseeMode(getRunningOverseeMode());
        setNightMode(getNightMode());
    }

    public boolean isFirstRun() {
        return settingMMKV.decodeBool(FIRST_RUN, false);
    }

    public void setFirstRun() {
        settingMMKV.encode(FIRST_RUN, true);
    }

    public boolean isServiceEnabled() {
        return settingMMKV.decodeBool(SERVICE_ENABLED, false);
    }

    public void setServiceEnabled(boolean enabled) {
        settingMMKV.encode(SERVICE_ENABLED, enabled);
    }

    public SortType getSortType() {
        return settingMMKV.decodeParcelable(SORT_TYPE, SortType.class, SortType.CREATE_TIME_ASC);
    }

    public void setSortType(SortType sortType) {
        settingMMKV.encode(SORT_TYPE, sortType);
    }

    public Comparator<Task> getComparator() {
        TaskRepository repository = TaskRepository.getInstance();
        SortType sortType = getSortType();
        return (o1, o2) -> {
            TaskConfig config1 = repository.getTaskConfig(o1.getId());
            TaskConfig config2 = repository.getTaskConfig(o2.getId());
            switch (sortType) {
                case CREATE_TIME_ASC:
                    return (config1.getCreateTime() - config2.getCreateTime()) < 0 ? -1 : 1;
                case CREATE_TIME_DESC:
                    return (config1.getCreateTime() - config2.getCreateTime()) > 0 ? -1 : 1;
                case MODIFY_TIME_ASC:
                    return (config1.getModifyTime() - config2.getModifyTime()) < 0 ? -1 : 1;
                case MODIFY_TIME_DESC:
                    return (config1.getModifyTime() - config2.getModifyTime()) > 0 ? -1 : 1;
            }
            return 0;
        };
    }

    public boolean isKeepAlive() {
        return settingMMKV.decodeBool(KEEP_ALIVE, false);
    }

    public void setKeepAlive(Context context, boolean keepAlive) {
        KeepAliveService aliveService = MainApplication.getAliveService();
        if (keepAlive && aliveService == null) {
            context.startService(new Intent(context, KeepAliveService.class));
        }
        if (!keepAlive && aliveService != null) {
            context.stopService(new Intent(context, KeepAliveService.class));
        }
        settingMMKV.encode(KEEP_ALIVE, keepAlive);
    }

    public int getActionTouchOffset() {
        return settingMMKV.decodeInt(ACTION_TOUCH_OFFSET, 10);
    }

    public Point getOffsetPosition(int x, int y) {
        int offset = getActionTouchOffset();
        return new Point((int) (Math.random() * offset * 2 + x - offset), (int) (Math.random() * offset * 2 + y - offset));
    }

    public void setActionTouchOffset(int offset) {
        settingMMKV.encode(ACTION_TOUCH_OFFSET, offset);
    }

    public int getActionRecordDelay() {
        return settingMMKV.decodeInt(ACTION_RECORD_DELAY, 300);
    }

    public void setActionRecordDelay(int delay) {
        settingMMKV.encode(ACTION_RECORD_DELAY, delay);
    }

    public int getEventTimeout() {
        return settingMMKV.decodeInt(EVENT_TIMEOUT, 100);
    }

    public void setEventTimeout(int delay) {
        settingMMKV.encode(EVENT_TIMEOUT, delay);
        MainAccessibilityService service = MainApplication.getService();
        if (service != null && service.isServiceConnected()) {
            service.setEventTimeout();
        }
    }


    public OverseeMode getRunningOverseeMode() {
        return settingMMKV.decodeParcelable(RUNNING_OVERSEE_MODE, OverseeMode.class, OverseeMode.CLOSED);
    }

    public void setRunningOverseeMode(OverseeMode overseeMode) {
        View view = EasyFloat.getView(PlayFloatView.class.getCanonicalName());
        if (view != null) ((PlayFloatView) view).setOverseeMode(overseeMode);
        settingMMKV.encode(RUNNING_OVERSEE_MODE, overseeMode);
    }

    public boolean isRunningLog() {
        return settingMMKV.decodeBool(RUNNING_LOG, false);
    }

    public void setRunningLog(boolean runningLog) {
        settingMMKV.encode(RUNNING_LOG, runningLog);
    }

    public boolean isRunningTaskInfo() {
        return settingMMKV.decodeBool(RUNNING_TASK_INFO, true);
    }

    public void setRunningTaskInfo(boolean taskInfo) {
        settingMMKV.encode(RUNNING_TASK_INFO, taskInfo);
    }

    public NightMode getNightMode() {
        return settingMMKV.decodeParcelable(NIGHT_MODE, NightMode.class, NightMode.AUTO);
    }

    public void setNightMode(NightMode nightMode) {
        settingMMKV.encode(NIGHT_MODE, nightMode);
        AppCompatDelegate.setDefaultNightMode(nightMode.getModeValue());
    }

    public boolean isDynamicColor() {
        return settingMMKV.decodeBool(DYNAMIC_COLOR, isDynamicColor);
    }

    public void setDynamicColor(Context context, boolean enabled) {
        if (!isAppliedDynamicColor) {
            isAppliedDynamicColor = true;
            isDynamicColor = enabled;
            DynamicColors.applyToActivitiesIfAvailable((Application) context.getApplicationContext(), options);
        } else {
            if (isDynamicColor != enabled) {
                isDynamicColor = enabled;
                MainApplication.getActivity().recreate();
            }
        }
        settingMMKV.encode(DYNAMIC_COLOR, enabled);
    }
}
