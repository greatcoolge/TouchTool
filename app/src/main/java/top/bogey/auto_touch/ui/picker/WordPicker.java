package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.accessibility.AccessibilityNodeInfo;
import android.view.accessibility.AccessibilityWindowInfo;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentPickerBgBinding;

public class WordPicker extends NodePicker{
    private boolean addEnabled = true;
    private WordPickerView wordPickerView;
    private String key = "";

    public WordPicker(@NonNull Context context, PickerCallback pickerCallback) {
        super(context, null, pickerCallback);
        FloatFragmentPickerBgBinding binding = FloatFragmentPickerBgBinding.inflate(LayoutInflater.from(context));
        layout = binding.getRoot();
        binding.closeButton.setOnClickListener(v -> {
            addEnabled = false;
            if (wordPickerView != null){
                key = wordPickerView.getWord();
            }
            if (pickerCallback != null){
                pickerCallback.call(this);
            }
            dismiss();
        });
        floatCallback = new TouchCallback();
    }

    public String getKey(){
        return key;
    }

    private void addWordView(int x, int y){
        AccessibilityNodeInfo nodeInfo = getNodeIn(x, y);
        nodeInfo = getRealNode(nodeInfo);
        if (nodeInfo != null){

            String key = null;
            CharSequence text = nodeInfo.getText();
            if (text != null && text.length() > 0) key = text.toString();
            else {
                String name = nodeInfo.getViewIdResourceName();
                if (name != null && !name.isEmpty()) key = name;
            }

            if (key != null){
                Rect rect = new Rect();
                nodeInfo.getBoundsInScreen(rect);
                if (wordPickerView != null) wordPickerView.dismiss();
                wordPickerView = new WordPickerView(context, nodePicker -> wordPickerView = null);
                wordPickerView.show(rect, key);
            }
        }

    }

    @Nullable
    private AccessibilityNodeInfo getNodeIn(int x, int y){
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
            List<AccessibilityWindowInfo> windows = service.getWindows();
            AccessibilityNodeInfo nodeInfo = service.getRootInActiveWindow();
            if (nodeInfo == null) return null;
            Map<Integer, AccessibilityNodeInfo> deepNodeInfo = new HashMap<>();
            findNodeIn(deepNodeInfo, 1, nodeInfo, x, y);
            return deepNodeInfo.get(deepNodeInfo.size());
        }
        return null;
    }

    private void findNodeIn(Map<Integer, AccessibilityNodeInfo> deepNodeInfo, int deep, @NonNull AccessibilityNodeInfo nodeInfo, int x, int y){
        if (nodeInfo.getChildCount() == 0) return;
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null){
                Rect rect = new Rect();
                child.getBoundsInScreen(rect);
                if (rect.contains(x, y)){
                    deepNodeInfo.put(deep, child);
                    findNodeIn(deepNodeInfo, deep + 1, child, x, y);
                }
            }
        }
    }

    private AccessibilityNodeInfo getRealNode(AccessibilityNodeInfo nodeInfo){
        if (nodeInfo == null) return null;
        CharSequence text = nodeInfo.getText();
        if (text != null && text.length() > 0) return nodeInfo;
        String name = nodeInfo.getViewIdResourceName();
        if (name != null && !name.isEmpty() && nodeInfo.isClickable()) return nodeInfo;
        return getRealNode(nodeInfo.getParent());
    }

    private class TouchCallback extends FloatPickerShowCallback{
        private boolean drag;
        private float lastX, lastY;

        @Override
        public void touchEvent(@NonNull View view, @NonNull MotionEvent motionEvent) {
            super.touchEvent(view, motionEvent);
            float rawX = motionEvent.getRawX();
            float rawY = motionEvent.getRawY();
            switch (motionEvent.getAction()) {
                case MotionEvent.ACTION_DOWN:
                    drag = false;
                    lastX = rawX;
                    lastY = rawY;
                case MotionEvent.ACTION_MOVE:
                    float dx = rawX - lastX;
                    float dy = rawY - lastY;
                    if (!drag && dx * dx + dy * dy < 81) break;
                    drag = true;
                    lastX = rawX;
                    lastY = rawY;
                case MotionEvent.ACTION_UP:
                    if (!drag){
                        layout.postDelayed(() -> {
                            if (addEnabled){
                                addWordView((int) rawX, (int) rawY);
                            }
                        }, 200);
                    }
            }
        }

        @Override
        public void dismiss() {
            super.dismiss();
            if (wordPickerView != null){
                wordPickerView.dismiss();
            }
        }
    }
}
