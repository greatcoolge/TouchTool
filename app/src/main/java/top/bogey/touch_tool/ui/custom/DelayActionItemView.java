package top.bogey.touch_tool.ui.custom;

import android.content.Context;
import android.graphics.Canvas;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.FloatActionDelayBinding;

public class DelayActionItemView extends FrameLayout {
    private FloatActionDelayBinding binding;

    private boolean isLock = true;
    private DelayTextWatcher watcher;

    public DelayActionItemView(@NonNull Context context) {
        super(context);
        init();
    }

    public DelayActionItemView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public DelayActionItemView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    private void init(){
        binding = FloatActionDelayBinding.inflate(LayoutInflater.from(getContext()), this, true);
        binding.lockButton.setOnClickListener(view -> {
            isLock = !isLock;
            binding.lockButton.setIconResource(isLock ? R.drawable.icon_lock : R.drawable.icon_unlock);
            binding.timeMax.setEnabled(!isLock);
            binding.timeMax.setText(binding.timeMin.getText());
        });

        binding.timeMin.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) {

            }

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) {

            }

            @Override
            public void afterTextChanged(Editable editable) {
                if (isLock){
                    binding.timeMax.setText(editable);
                }
                if (watcher != null) watcher.afterTextChanged(true, editable);
            }
        });

        binding.timeMax.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) {

            }

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) {

            }

            @Override
            public void afterTextChanged(Editable editable) {
                if (watcher != null) watcher.afterTextChanged(false, editable);
            }
        });
    }

    public void setTextWatcher(DelayTextWatcher watcher){
        this.watcher = watcher;
    }

    public void setValue(int min, int max){
        isLock = min == max;
        binding.lockButton.setIconResource(isLock ? R.drawable.icon_lock : R.drawable.icon_unlock);
        binding.timeMax.setEnabled(!isLock);
        binding.timeMin.setText(String.valueOf(min));
        binding.timeMax.setText(String.valueOf(max));
    }

    @Override
    protected boolean drawChild(Canvas canvas, View child, long drawingTime) {
        return super.drawChild(canvas, child, drawingTime);
    }

    public interface DelayTextWatcher{
        void afterTextChanged(boolean isMin, Editable editable);
    }
}
