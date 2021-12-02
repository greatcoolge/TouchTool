package top.bogey.auto_touch.ui.picker;

import android.view.View;

import androidx.annotation.Nullable;

import com.lzf.easyfloat.EasyFloat;

import top.bogey.auto_touch.ui.action.ActionEditDialog;
import top.bogey.auto_touch.ui.record.TaskRecordDialog;

public class FloatPickerShowCallback extends FloatCallback{
    @Override
    public void createdResult(boolean b, @Nullable String s, @Nullable View view) {
        super.createdResult(b, s, view);
        if (b){
            boolean actionEditExist = EasyFloat.getFloatView(ActionEditDialog.class.getCanonicalName()) != null;
            if (actionEditExist){
                EasyFloat.hide(ActionEditDialog.class.getCanonicalName());
            } else {
                EasyFloat.hide(TaskRecordDialog.class.getCanonicalName());
            }
        }
    }

    @Override
    public void dismiss() {
        super.dismiss();
        boolean actionEditExist = EasyFloat.getFloatView(ActionEditDialog.class.getCanonicalName()) != null;
        if (actionEditExist){
            EasyFloat.show(ActionEditDialog.class.getCanonicalName());
        } else {
            EasyFloat.show(TaskRecordDialog.class.getCanonicalName());
        }
    }
}
