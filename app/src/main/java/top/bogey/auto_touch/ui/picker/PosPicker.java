package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.widget.TextView;

import androidx.annotation.NonNull;

import top.bogey.auto_touch.R;

public class PosPicker extends NodePicker{
    public PosPicker(@NonNull Context context, PickerCallback pickerCallback) {
        super(context, R.layout.float_fragment_picker_pos, pickerCallback);
        floatCallback = new FloatClickCallback(this);
    }

    public void setIndex(int index){
        TextView textView = layout.findViewById(R.id.index_text);
        textView.setText(String.valueOf(index));
    }
}
