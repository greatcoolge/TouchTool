package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.MotionEvent;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.util.AppUtil;

public class PosPicker extends NodePicker{
    private final List<PosPickerView> posViews = new ArrayList<>();
    private final List<Pos> posList = new ArrayList<>();
    private boolean addEnabled = true;

    public PosPicker(@NonNull Context context, PickerCallback pickerCallback) {
        super(context, null, pickerCallback);
        layout = new PosPickerView2(context, (nodePicker) -> {

        });
        floatCallback = new TouchPickerCallback();
    }

    public List<Pos> getPosList() {
        return posList;
    }

    private class TouchPickerCallback extends FloatShowPickerCallback {
        @Override
        public void onTouch(MotionEvent motionEvent) {
            super.onTouch(motionEvent);
            ((PosPickerView2) layout).onTouch(motionEvent);
        }
    }
}
