package top.bogey.auto_touch.ui.picker;

import top.bogey.auto_touch.ui.action.FloatActionEdit;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.record.TaskRecordDialog;

public class FloatPickerShowCallback extends FloatCallbackImpl {
    @Override
    public void onCreate(boolean succeed) {
        if (succeed){
            boolean actionEditExist = EasyFloat.getView(FloatActionEdit.class.getCanonicalName()) != null;
            if (actionEditExist){
                EasyFloat.hide(FloatActionEdit.class.getCanonicalName());
            } else {
                EasyFloat.hide(TaskRecordDialog.class.getCanonicalName());
            }
        }
    }

    @Override
    public void onDismiss() {
        boolean actionEditExist = EasyFloat.getView(FloatActionEdit.class.getCanonicalName()) != null;
        if (actionEditExist){
            EasyFloat.show(FloatActionEdit.class.getCanonicalName());
        } else {
            EasyFloat.show(TaskRecordDialog.class.getCanonicalName());
        }
    }
}
