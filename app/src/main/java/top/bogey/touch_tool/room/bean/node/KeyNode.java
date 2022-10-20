package top.bogey.touch_tool.room.bean.node;

import android.accessibilityservice.AccessibilityService;
import android.content.Context;
import android.os.Build;
import android.widget.Toast;

import androidx.lifecycle.ViewModelProvider;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.ui.apps.AppInfo;
import top.bogey.touch_tool.utils.AppUtils;

public class KeyNode extends Node{
    public KeyNode(KeyTask value) {
        super(NodeType.KEY, value);
    }

    public KeyNode(KeyType type) {
        super(NodeType.KEY);
        setValue(new KeyTask(type));
    }

    @Override
    public KeyTask getValue() {
        return (KeyTask) value;
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public boolean checkNode(Object obj) {
        return true;
    }

    @Override
    public Object getNodeTarget(Object obj) {
        return getValue();
    }

    @Override
    public KeyTask cloneValue() {
        return getValue();
    }

    public static class KeyTask{
        private final KeyType keyType;
        private String extras;

        public KeyTask(KeyType keyType) {
            this.keyType = keyType;
        }

        public KeyTask(KeyType keyType, String extras) {
            this.keyType = keyType;
            this.extras = extras;
        }

        public KeyType getKeyType() {
            return keyType;
        }

        public String getExtras() {
            return extras;
        }

        public void setExtras(String extras) {
            this.extras = extras;
        }
    }

    public enum KeyType {
        BACK,
        HOME,
        TASK,
        WEAK,
        LOCK,
        SNAP,
        GOTO;

        public boolean doKeyTask(AccessibilityService service, String extras){
            switch (this){
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
                        Toast.makeText(service,  R.string.action_device_not_support_snap, Toast.LENGTH_SHORT).show();
                        return false;
                    }
                case GOTO:
                    AppUtils.gotoApp(service, extras);
                    return true;

            }
            return false;
        }

        public String getTitle(Context context, String extras){
            int key = 0;
            switch (this) {
                case BACK:
                    key = R.string.key_target_back;
                    break;
                case HOME:
                    key = R.string.key_target_home;
                    break;
                case TASK:
                    key = R.string.key_target_task;
                    break;
                case WEAK:
                    key = R.string.key_target_weak;
                    break;
                case LOCK:
                    key = R.string.key_target_lock;
                    break;
                case SNAP:
                    key = R.string.key_target_snap;
                    break;
                case GOTO:
                    key = R.string.key_target_goto;
                    break;
            }
            if (key != 0) {
                if (this == GOTO) {
                    MainViewModel viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
                    AppInfo appInfo = viewModel.getAppInfoByPkgName(extras);
                    if (appInfo != null) return context.getString(key, appInfo.appName);
                    return context.getString(key, extras);
                }
                return context.getString(key);
            }
            return "";
        }
    }
}
