package top.bogey.auto_touch.ui.picker;

import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.record.TaskRecordDialog;

public class FloatActionEditShowCallback extends FloatShowCallback{
    @Override
    public void onCreate(boolean succeed) {
        if (succeed){
            boolean taskRecordExist = EasyFloat.getView(TaskRecordDialog.class.getCanonicalName()) != null;
            if (taskRecordExist){
                EasyFloat.hide(TaskRecordDialog.class.getCanonicalName());
            } else {
                super.onCreate(true);
            }
        }
    }

    @Override
    public void onDismiss() {
        boolean taskRecordExist = EasyFloat.getView(TaskRecordDialog.class.getCanonicalName()) != null;
        if (taskRecordExist){
            EasyFloat.show(TaskRecordDialog.class.getCanonicalName());
        } else {
            super.onDismiss();
        }
    }
}
