package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.graphics.Rect;
import android.view.Gravity;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityNodeInfo;
import android.widget.TextView;

import androidx.annotation.NonNull;

import java.util.HashMap;
import java.util.Map;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;

public class WordPicker extends NodePicker{
    public WordPicker(@NonNull Context context, PickerCallback pickerCallback) {
        super(context, R.layout.float_fragment_picker_word, pickerCallback);
        floatCallback = new FloatClickCallback(this);
    }

    @Override
    public void show(int gravity, int x, int y) {
        AccessibilityNodeInfo nodeInfo = getNodeIn(x, y);
        nodeInfo = getRealNode(nodeInfo);
        if (nodeInfo != null){
            Rect rect = new Rect();
            nodeInfo.getBoundsInScreen(rect);
            super.show(Gravity.START | Gravity.TOP, rect.left, rect.top);
            ViewGroup.LayoutParams params = layout.getLayoutParams();
            params.width = rect.width();
            params.height = rect.height();
            layout.setLayoutParams(params);

            TextView textView = layout.findViewById(R.id.title_text);
            CharSequence text = nodeInfo.getText();
            String name = nodeInfo.getViewIdResourceName();
            if (text != null && text.length() > 0) textView.setText(text);
            else if (name != null && !name.isEmpty()) textView.setText(name);
        }
    }

    public String getWord(){
        TextView textView = layout.findViewById(R.id.title_text);
        return String.valueOf(textView.getText());
    }

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

    private void findNodeIn(Map<Integer, AccessibilityNodeInfo> deepNodeInfo, int deep, AccessibilityNodeInfo nodeInfo, int x, int y){
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
}
