package top.bogey.auto_touch.ui.picker;

import android.view.View;

import androidx.annotation.Nullable;

import com.lzf.easyfloat.EasyFloat;

import top.bogey.auto_touch.ui.record.TaskRecordDialog;

public class FloatActionEditShowCallback extends FloatShowCallback{
    @Override
    public void createdResult(boolean b, @Nullable String s, @Nullable View view) {
        if (b){
            boolean taskRecordExist = EasyFloat.getFloatView(TaskRecordDialog.class.getCanonicalName()) != null;
            if (taskRecordExist){
                EasyFloat.hide(TaskRecordDialog.class.getCanonicalName());
            } else {
                super.createdResult(true, s, view);
            }
        }
    }

    @Override
    public void dismiss() {
        boolean taskRecordExist = EasyFloat.getFloatView(TaskRecordDialog.class.getCanonicalName()) != null;
        if (taskRecordExist){
            EasyFloat.show(TaskRecordDialog.class.getCanonicalName());
        } else {
            super.dismiss();
        }
    }
}
