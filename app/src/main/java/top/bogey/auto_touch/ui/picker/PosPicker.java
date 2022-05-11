package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.view.MotionEvent;

import androidx.annotation.NonNull;

import java.util.List;

import top.bogey.auto_touch.room.bean.Pos;

public class PosPicker extends NodePicker{
    public PosPicker(@NonNull Context context, PickerCallback pickerCallback, List<Pos> poses) {
        super(context, null, pickerCallback);
        layout = new PosPickerView(context, nodePicker -> {
            pickerCallback.call(nodePicker);
            dismiss();
        }, poses);
        floatCallback = new TouchPickerCallback();
    }

    private class TouchPickerCallback extends FloatShowPickerCallback {
        @Override
        public void onTouch(MotionEvent motionEvent) {
            super.onTouch(motionEvent);
            ((PosPickerView) layout).onTouch(motionEvent);
        }
    }
}
