package top.bogey.touch_tool.database.bean.action;

import android.accessibilityservice.AccessibilityService;
import android.content.Context;
import android.os.Build;
import android.os.Parcel;
import android.os.Parcelable;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;
import top.bogey.touch_tool.ui.app.AppInfo;
import top.bogey.touch_tool.utils.AppUtils;

public class SystemAction extends Action {
    private SystemActionType systemActionType;
    private String extras;

    public SystemAction() {
        super(ActionType.SYSTEM);
        systemActionType = SystemActionType.BACK;
    }

    protected SystemAction(Parcel in) {
        super(ActionType.SYSTEM);
        systemActionType = in.readParcelable(SystemActionType.class.getClassLoader());
        extras = in.readString();
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        return true;
    }

    @Override
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        if (!super.doAction(task, service, runningInfo)) return false;

        switch (systemActionType) {
            case BACK:
                service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_BACK);
                return true;
            case HOME:
                service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_HOME);
                return true;
            case TASK:
                service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_RECENTS);
                return true;
            case LOCK:
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                    service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_LOCK_SCREEN);
                    return true;
                } else {
                    Toast.makeText(service, R.string.action_device_not_support_lock, Toast.LENGTH_SHORT).show();
                    return false;
                }
            case WEAK:
                AppUtils.wakeScreen(service);
                return true;
            case SNAP:
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                    service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_TAKE_SCREENSHOT);
                    return true;
                } else {
                    Toast.makeText(service, R.string.action_device_not_support_snap, Toast.LENGTH_SHORT).show();
                    return false;
                }
            case GOTO:
                AppUtils.gotoApp(service, extras);
                return true;

        }
        return false;
    }

    @Override
    public String getDescription(Context context, Task task, Behavior behavior) {
        if (context == null) return systemActionType.name();
        return systemActionType.getDescription(context, extras);
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        super.writeToParcel(dest, flags);
        dest.writeParcelable(systemActionType, flags);
        dest.writeString(extras);
    }

    public SystemActionType getSystemActionType() {
        return systemActionType;
    }

    public void setSystemActionType(SystemActionType systemActionType) {
        this.systemActionType = systemActionType;
    }

    public String getExtras() {
        return extras;
    }

    public void setExtras(String extras) {
        this.extras = extras;
    }

    public enum SystemActionType implements Parcelable {
        BACK,
        HOME,
        TASK,
        WEAK,
        LOCK,
        SNAP,
        GOTO;

        public String getDescription(Context context, String extras) {
            int key = 0;
            switch (this) {
                case BACK:
                    key = R.string.action_system_back;
                    break;
                case HOME:
                    key = R.string.action_system_home;
                    break;
                case TASK:
                    key = R.string.action_system_task;
                    break;
                case WEAK:
                    key = R.string.action_system_weak;
                    break;
                case LOCK:
                    key = R.string.action_system_lock;
                    break;
                case SNAP:
                    key = R.string.action_system_snap;
                    break;
                case GOTO:
                    key = R.string.action_system_goto;
                    break;
            }
            if (key != 0) {
                if (this == GOTO) {
                    MainViewModel viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
                    AppInfo appInfo = viewModel.getAppInfoByPkgName(extras);
                    if (appInfo != null) return context.getString(key, appInfo.appName);
                    return context.getString(key, extras).trim();
                }
                return context.getString(key);
            }
            return "";
        }

        public static final Creator<SystemActionType> CREATOR = new Creator<SystemActionType>() {
            @Override
            public SystemActionType createFromParcel(Parcel in) {
                return SystemActionType.values()[in.readByte()];
            }

            @Override
            public SystemActionType[] newArray(int size) {
                return new SystemActionType[size];
            }
        };

        @Override
        public int describeContents() {
            return 0;
        }

        @Override
        public void writeToParcel(@NonNull Parcel dest, int flags) {
            dest.writeByte((byte) ordinal());
        }
    }
}
