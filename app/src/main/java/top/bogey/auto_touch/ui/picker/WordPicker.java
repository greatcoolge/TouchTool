package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentPickerBgBinding;

public class WordPicker extends NodePicker{
    private final FloatFragmentPickerBgBinding binding;
    private boolean addEnabled = true;
    private WordPickerView wordPickerView;
    private final List<WordPickerView> wordPickerViews = new ArrayList<>();
    private String key = "";

    public WordPicker(@NonNull Context context, PickerCallback pickerCallback) {
        super(context, null, pickerCallback);
        binding = FloatFragmentPickerBgBinding.inflate(LayoutInflater.from(context));
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

    @Override
    public void show(int gravity, int x, int y) {
        super.show(gravity, x, y);
        markAll();
    }

    public String getKey(){
        if (key.isEmpty()) return key;
        return String.format("\"%s\"", key);
    }

    private void addWordView(int x, int y){
        AccessibilityNodeInfo nodeInfo = getNodeIn(x, y);
        nodeInfo = getClickableParent(nodeInfo);
        if (nodeInfo != null){

            String key = getNodeText(nodeInfo);
            if (key != null && !key.isEmpty()){
                Rect rect = new Rect();
                nodeInfo.getBoundsInScreen(rect);
                if (wordPickerView != null) wordPickerView.dismiss();
                wordPickerView = new WordPickerView(context, nodePicker -> wordPickerView = null);
                wordPickerView.show(rect, key);
                for (WordPickerView view : wordPickerViews) {
                    view.refreshSelectState(view == wordPickerView);
                }
            }
        }
    }

    private void markAll(){
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
            AccessibilityNodeInfo nodeInfo = service.getRootInActiveWindow();
            List<AccessibilityNodeInfo> clickableChildren =  new ArrayList<>();
            getClickableChildren(clickableChildren, nodeInfo);
            for (AccessibilityNodeInfo child : clickableChildren) {
                String name = getNodeText(child);
                if (name != null && !name.isEmpty()){
                    Rect rect = new Rect();
                    child.getBoundsInScreen(rect);
                    WordPickerView wordPickerView = new WordPickerView(context, nodePicker -> {
                        WordPickerView pickerView = (WordPickerView) nodePicker;
                        if (this.wordPickerView != null){
                            this.wordPickerView.dismiss();
                        }
                        if (pickerView == this.wordPickerView){
                            this.wordPickerView = null;
                        } else {
                            this.wordPickerView = pickerView;
                        }
                        for (WordPickerView view : wordPickerViews) {
                            view.refreshSelectState(view == this.wordPickerView);
                        }
                        addEnabled = false;
                    });
                    wordPickerView.init(rect, name, true);
                    binding.markBox.addView(wordPickerView);
                    wordPickerView.setX(rect.left);
                    wordPickerView.setY(rect.top);
                    wordPickerViews.add(wordPickerView);
                }
            }
        }
    }

    private String getNodeText(AccessibilityNodeInfo nodeInfo){
        if (nodeInfo != null){
            String name = nodeInfo.getViewIdResourceName();
            if (name == null || name.isEmpty()){
                CharSequence text = nodeInfo.getText();
                if (text != null && text.length() > 0){
                    name = text.toString();
                }
            }
            if (name == null || name.isEmpty()){
                for (int i = 0; i < nodeInfo.getChildCount(); i++) {
                    AccessibilityNodeInfo child = nodeInfo.getChild(i);
                    if (child != null){
                        name = getNodeText(child);
                        if (name != null && !name.isEmpty()){
                            return name;
                        }
                    }
                }
            }
            return name;
        }
        return null;
    }

    @Nullable
    private AccessibilityNodeInfo getNodeIn(int x, int y){
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
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

    private AccessibilityNodeInfo getClickableParent(AccessibilityNodeInfo nodeInfo){
        if (nodeInfo == null) return null;
        if (nodeInfo.isClickable()) return nodeInfo;
        return getClickableParent(nodeInfo.getParent());
    }

    private boolean getClickableChildren(List<AccessibilityNodeInfo> nodes, AccessibilityNodeInfo nodeInfo){
        if (nodeInfo == null) return false;
        if (nodeInfo.getChildCount() == 0) return false;
        int size = nodes.size();
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null){
                boolean exist = getClickableChildren(nodes, child);
                if (!exist && (child.isClickable() || child.isCheckable()) && child.isVisibleToUser()){
                    nodes.add(child);
                }
            }
        }
        return size != nodes.size();
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
                            addEnabled = true;
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
