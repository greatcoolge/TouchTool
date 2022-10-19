package top.bogey.touch_tool.room.bean.node;

import android.accessibilityservice.AccessibilityService;
import android.os.Build;

import top.bogey.touch_tool.utils.AppUtils;

public class KeyNode extends Node{
    public KeyNode(Integer value) {
        super(NodeType.KEY, value);
    }

    @Override
    public Integer getValue() {
        return (Integer) value;
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
    public Integer cloneValue() {
        return getValue();
    }

    public enum KeyType {
        BACK,
        HOME,
        TASK,
        LOCK,
        WEAK,
        SNAP;

        public void doKey(AccessibilityService service){
            switch (this){
                case BACK:
                    service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_BACK);
                    break;
                case HOME:
                    service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_HOME);
                    break;
                case TASK:
                    service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_RECENTS);
                    break;
                case LOCK:
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                        service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_LOCK_SCREEN);
                    }
                    break;
                case WEAK:
                    AppUtils.wakeScreen(service);
                    break;
                case SNAP:
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                        service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_TAKE_SCREENSHOT);
                    }
                    break;
            }
        }
    }
}
