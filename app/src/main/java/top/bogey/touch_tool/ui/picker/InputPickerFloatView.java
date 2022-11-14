package top.bogey.touch_tool.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.database.bean.action.InputAction;
import top.bogey.touch_tool.databinding.FloatPickerInputBinding;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.FloatBaseCallback;

@SuppressLint("ViewConstructor")
public class InputPickerFloatView extends BasePickerFloatView {
    private final FloatPickerInputBinding binding;
    private final InputAction inputAction;

    private AccessibilityNodeInfo rootNode;
    private AccessibilityNodeInfo selectNode = null;

    private String selectKey = "";
    private String selectLevel = "";

    public InputPickerFloatView(@NonNull Context context, PickerCallback pickerCallback, InputAction inputAction) {
        super(context, pickerCallback);
        this.inputAction = inputAction;

        binding = FloatPickerInputBinding.inflate(LayoutInflater.from(context), this, true);

        floatCallback = new WordPickerCallback();

        binding.saveButton.setOnClickListener(v -> {
            if (pickerCallback != null) {
                pickerCallback.onComplete(this);
            }
            dismiss();
        });

        binding.backButton.setOnClickListener(v -> dismiss());

        binding.titleText.setOnClickListener(v -> {
            CharSequence text = binding.titleText.getText();
            if (text != null && text.length() > 0) {
                if (text.toString().equals(selectKey)) {
                    binding.titleText.setText(selectLevel);
                } else {
                    binding.titleText.setText(selectKey);
                }
            } else {
                binding.titleText.setText(selectKey);
            }
        });

        binding.markBox.setOnClickListener(v -> showInputView(null));
    }

    public InputAction getInput() {
        InputAction action = new InputAction();
        if (selectNode != null) {
            CharSequence text = binding.titleText.getText();
            if (text != null && text.length() > 0) {
                action.setId(String.format("\"%s\"", text));
            }
        }
        return action;
    }

    private void markAll() {
        MainAccessibilityService service = MainApplication.getService();
        if (service != null) {
            rootNode = service.getRootInActiveWindow();
        }
    }

    public void showInputView(AccessibilityNodeInfo nodeInfo) {
        if (nodeInfo == null) {
            selectNode = null;
            selectKey = "";
            selectLevel = "";
            binding.markBox.setVisibility(INVISIBLE);
        } else {
            selectNode = nodeInfo;
            selectLevel = "lv/" + getNodeLevel(nodeInfo);
            selectKey = getNodeKey(nodeInfo);
            if (selectKey.isEmpty()) selectKey = selectLevel;

            binding.titleText.setText(selectKey);

            Rect rect = new Rect();
            nodeInfo.getBoundsInScreen(rect);
            int[] location = new int[2];
            getLocationOnScreen(location);
            ViewGroup.LayoutParams params = binding.markBox.getLayoutParams();
            params.width = Math.max(rect.width(), DisplayUtils.dp2px(getContext(), 30));
            params.height = Math.max(rect.height(), DisplayUtils.dp2px(getContext(), 40));
            binding.markBox.setLayoutParams(params);
            binding.markBox.setX(rect.left);
            binding.markBox.setY(rect.top - location[1]);
            binding.markBox.setVisibility(VISIBLE);
        }
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        float x = event.getRawX();
        float y = event.getRawY();

        if (event.getAction() == MotionEvent.ACTION_UP) {
            AccessibilityNodeInfo node = getInputNodeIn((int) x, (int) y);
            if (node != null) {
                showInputView(node);
            }
        }
        return true;
    }

    private String getNodeText(AccessibilityNodeInfo nodeInfo) {
        String name = "";
        if (nodeInfo != null) {
            String resourceName = nodeInfo.getViewIdResourceName();
            if (resourceName != null && !resourceName.isEmpty()) {
                name = resourceName;
            }
        }
        return name;
    }

    private String getNodeKey(AccessibilityNodeInfo nodeInfo) {
        String key = getNodeText(nodeInfo);
        Pattern pattern = Pattern.compile(".+:(id/.+)");
        Matcher matcher = pattern.matcher(key);
        if (matcher.find() && matcher.group(1) != null) {
            key = matcher.group(1);
        }
        return key;
    }

    private String getNodeLevel(AccessibilityNodeInfo nodeInfo) {
        AccessibilityNodeInfo parent = nodeInfo.getParent();
        if (parent != null) {
            for (int i = 0; i < parent.getChildCount(); i++) {
                AccessibilityNodeInfo child = parent.getChild(i);
                if (child != null && child.equals(nodeInfo)) {
                    String level = getNodeLevel(parent);
                    if (!level.isEmpty()) {
                        return level + "," + i;
                    } else {
                        return String.valueOf(i);
                    }
                }
            }
        }
        return "";
    }

    @Nullable
    private AccessibilityNodeInfo getInputNodeIn(int x, int y) {
        if (rootNode == null) return null;
        Map<Integer, AccessibilityNodeInfo> deepNodeInfo = new HashMap<>();
        findInputNodeIn(deepNodeInfo, 1, rootNode, x, y);
        int max = 0;
        AccessibilityNodeInfo node = null;
        for (Map.Entry<Integer, AccessibilityNodeInfo> entry : deepNodeInfo.entrySet()) {
            if (max == 0 || entry.getKey() > max) {
                max = entry.getKey();
                node = entry.getValue();
            }
        }
        return node;
    }

    private void findInputNodeIn(Map<Integer, AccessibilityNodeInfo> deepNodeInfo, int deep, @NonNull AccessibilityNodeInfo nodeInfo, int x, int y) {
        if (nodeInfo.getChildCount() == 0) return;
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null) {
                Rect rect = new Rect();
                child.getBoundsInScreen(rect);
                if (rect.contains(x, y)) {
                    if (child.isEditable()) {
                        deepNodeInfo.put(deep, child);
                    }
                    findInputNodeIn(deepNodeInfo, deep + 1, child, x, y);
                }
            }
        }
    }

    protected class WordPickerCallback extends FloatBaseCallback {
        @Override
        public void onCreate(boolean succeed) {
            super.onCreate(succeed);
            if (succeed) {
                markAll();
                showInputView(inputAction.searchInputBox(inputAction.searchNodes(rootNode)));
            }
        }
    }

}
