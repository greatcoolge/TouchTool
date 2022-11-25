package top.bogey.touch_tool.database.bean.action;

import android.accessibilityservice.AccessibilityService;
import android.content.Context;
import android.os.Build;
import android.os.Looper;
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

        boolean result = true;
        switch (systemActionType) {
            case BACK:
                service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_BACK);
                break;
            case HOME:
                service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_HOME);
                break;
            case TASK:
                service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_RECENTS);
                break;
            case SCREEN_LOCK:
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                    service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_LOCK_SCREEN);
                } else {
                    Toast.makeText(service, R.string.action_device_not_support_lock, Toast.LENGTH_SHORT).show();
                    result = false;
                }
                break;
            case SCREEN_WEAK:
                AppUtils.wakeScreen(service);
                break;
            case TAKE_SCREENSHOT:
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                    service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_TAKE_SCREENSHOT);
                } else {
                    Toast.makeText(service, R.string.action_device_not_support_snap, Toast.LENGTH_SHORT).show();
                    result = false;
                }
                break;
            case GOTO_APP:
                AppUtils.gotoApp(service, extras);
                break;
            case NOTIFICATION:
                service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_NOTIFICATIONS);
                break;
            case OPEN_CAPTURE:
                Boolean[] booleans = new Boolean[1];
                service.startCaptureService(true, result1 -> booleans[0] = result1);
                int count = 0;
                while (booleans[0] == null && count < 200) {
                    sleep(50);
                    count++;
                }
                if (booleans[0] != null) result = booleans[0];
                else result = false;
                break;
            case CLOSE_CAPTURE:
                service.stopCaptureService();
                break;
            case TOAST:
                Looper.prepare();
                Toast.makeText(service, extras, Toast.LENGTH_SHORT).show();
                Looper.loop();
                break;
        }
        sleep(timeArea.getRandomTime());
        return result;
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
        SCREEN_WEAK,
        SCREEN_LOCK,
        TAKE_SCREENSHOT,
        GOTO_APP,
        NOTIFICATION,
        OPEN_CAPTURE,
        CLOSE_CAPTURE,
        TOAST;

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
                case SCREEN_WEAK:
                    key = R.string.action_system_screen_weak;
                    break;
                case SCREEN_LOCK:
                    key = R.string.action_system_screen_lock;
                    break;
                case TAKE_SCREENSHOT:
                    key = R.string.action_system_take_screen_shot;
                    break;
                case NOTIFICATION:
                    key = R.string.action_system_notification;
                    break;
                case OPEN_CAPTURE:
                    key = R.string.action_system_open_capture;
                    break;
                case CLOSE_CAPTURE:
                    key = R.string.action_system_close_capture;
                    break;
                case TOAST:
                    key = R.string.action_system_toast;
                    break;
                case GOTO_APP:
                    key = R.string.action_system_goto_app;
                    break;
            }
            if (key != 0) {
                if (this == GOTO_APP) {
                    MainViewModel viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
                    AppInfo appInfo = viewModel.getAppInfoByPkgName(extras);
                    if (appInfo != null) return context.getString(key, appInfo.appName);
                    return context.getString(key, extras).trim();
                }
                if (this == TOAST) {
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
