package top.bogey.touch_tool.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityNodeInfo;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.amrdeveloper.treeview.TreeNode;
import com.amrdeveloper.treeview.TreeNodeManager;
import com.google.android.material.bottomsheet.BottomSheetBehavior;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.database.bean.action.TextAction;
import top.bogey.touch_tool.databinding.FloatPickerWordBinding;
import top.bogey.touch_tool.utils.FloatBaseCallback;

@SuppressLint("ViewConstructor")
public class WordPickerFloatView extends BasePickerFloatView {
    protected final FloatPickerWordBinding binding;
    private final TextAction textNode;

    private TreeNodeManager manager;

    private AccessibilityNodeInfo rootNode;

    private String selectKey = "";
    private String selectLevel = "";
    private String selectText = "";
    private AccessibilityNodeInfo selectNode;

    private String innerSelectKey = "";
    private String innerSelectLevel = "";
    private String innerSelectText = "";
    private AccessibilityNodeInfo innerNode;

    private String currSelect = "";

    public WordPickerFloatView(@NonNull Context context, PickerCallback pickerCallback, TextAction textNode) {
        super(context, pickerCallback);
        this.textNode = textNode;

        binding = FloatPickerWordBinding.inflate(LayoutInflater.from(context), this, true);

        floatCallback = new WordPickerCallback();

        binding.saveButton.setOnClickListener(v -> {
            if (pickerCallback != null) {
                pickerCallback.onComplete(this);
            }
            dismiss();
        });

        binding.backButton.setOnClickListener(v -> dismiss());

        binding.clickableTitleText.setOnClickListener(v -> {
            selectNext();
            binding.clickableTitleText.setText(currSelect);
            binding.clickableTitleText.setChecked(true);
            binding.innerTitleText.setChecked(false);
            selectTreeNode(selectNode);
        });

        binding.innerTitleText.setOnClickListener(v -> {
            selectInnerNext();
            binding.innerTitleText.setText(currSelect);
            binding.innerTitleText.setChecked(true);
            binding.clickableTitleText.setChecked(false);
            selectTreeNode(innerNode);
        });

        binding.clickableMarkBox.setOnClickListener(v -> showWordView(null, false));

        BottomSheetBehavior<FrameLayout> sheetBehavior = BottomSheetBehavior.from(binding.bottomSheet);
        sheetBehavior.addBottomSheetCallback(new BottomSheetBehavior.BottomSheetCallback() {
            @Override
            public void onStateChanged(@NonNull View bottomSheet, int newState) {
                if (newState == BottomSheetBehavior.STATE_COLLAPSED) {
                    bottomSheet.animate().alpha(0.2f);
                } else {
                    bottomSheet.animate().alpha(1f);
                }
            }

            @Override
            public void onSlide(@NonNull View bottomSheet, float slideOffset) {

            }
        });
    }

    public String getWord() {
        if (currSelect != null) {
            return String.format("\"%s\"", currSelect);
        }
        return "";
    }

    protected void markAll() {
        MainAccessibilityService service = MainApplication.getService();
        if (service != null) {
            if (manager == null) {
                manager = new TreeNodeManager();
                WordPickerTreeAdapter adapter = new WordPickerTreeAdapter(manager, this);
                binding.wordRecyclerView.setAdapter(adapter);
            }
            rootNode = service.getRootInActiveWindow();
            WordPickerTreeAdapter adapter = (WordPickerTreeAdapter) binding.wordRecyclerView.getAdapter();
            if (adapter != null) adapter.setRoot(rootNode);
        }
    }

    public void showWordView(AccessibilityNodeInfo nodeInfo, boolean selectTreeNode) {
        currSelect = null;
        binding.clickableMarkBox.setVisibility(INVISIBLE);
        binding.clickableTitleText.setVisibility(INVISIBLE);
        binding.innerMarkBox.setVisibility(INVISIBLE);
        binding.innerTitleText.setVisibility(INVISIBLE);
        if (nodeInfo != null) {
            binding.clickableMarkBox.setVisibility(VISIBLE);
            binding.clickableTitleText.setVisibility(VISIBLE);
            binding.innerMarkBox.setVisibility(VISIBLE);
            binding.innerTitleText.setVisibility(VISIBLE);

            selectNode = getClickableNode(nodeInfo);
            if (selectNode == null) return;
            innerNode = nodeInfo;

            setNodeSelectInfo(selectNode);
            setInnerNodeSelectInfo(innerNode);

            selectInnerNext();
            binding.innerTitleText.setText(currSelect);
            binding.innerTitleText.setChecked(false);
            selectNext();
            binding.clickableTitleText.setText(currSelect);
            binding.clickableTitleText.setChecked(true);

            int[] location = new int[2];
            getLocationOnScreen(location);

            Rect rect = new Rect();

            selectNode.getBoundsInScreen(rect);
            ViewGroup.LayoutParams params = binding.clickableMarkBox.getLayoutParams();
            params.width = rect.width();
            params.height = rect.height();
            binding.clickableMarkBox.setLayoutParams(params);
            binding.clickableMarkBox.setX(rect.left);
            binding.clickableMarkBox.setY(rect.top - location[1]);

            innerNode.getBoundsInScreen(rect);
            ViewGroup.LayoutParams innerParams = binding.innerMarkBox.getLayoutParams();
            innerParams.width = rect.width();
            innerParams.height = rect.height();
            binding.innerMarkBox.setLayoutParams(innerParams);
            binding.innerMarkBox.setX(rect.left);
            binding.innerMarkBox.setY(rect.top - location[1]);

            if (selectTreeNode) {
                selectTreeNode(selectNode);
            }
        }
    }

    private void selectTreeNode(AccessibilityNodeInfo nodeInfo) {
        if (nodeInfo == null) return;
        WordPickerTreeAdapter adapter = (WordPickerTreeAdapter) binding.wordRecyclerView.getAdapter();
        if (adapter != null) {
            adapter.collapseAll();
            if (manager.size() > 0) {
                TreeNode treeNode = manager.get(0);
                TreeNode node = findTreeNode(treeNode, nodeInfo);
                if (node != null) {
                    node.setSelected(true);
                    adapter.setSelectedNode(node);
                    TreeNode parent = node.getParent();
                    while (parent != null) {
                        TreeNode p = parent.getParent();
                        if (p != null) {
                            parent.setExpanded(true);
                            parent = p;
                        } else {
                            adapter.expandNode(parent);
                            parent = null;
                        }
                    }
                }
            }
        }
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        float x = event.getRawX();
        float y = event.getRawY();

        if (event.getAction() == MotionEvent.ACTION_UP) {
            AccessibilityNodeInfo node = getNodeIn((int) x, (int) y);
            if (node != null) {
                showWordView(node, true);
            }
        }
        return true;
    }

    private TreeNode findTreeNode(TreeNode treeNode, Object value) {
        if (value.equals(treeNode.getValue())) return treeNode;
        for (TreeNode child : treeNode.getChildren()) {
            TreeNode node = findTreeNode(child, value);
            if (node != null) return node;
        }
        return null;
    }

    private void selectNext() {
        List<String> strings = new ArrayList<>(Arrays.asList(selectKey, selectLevel, selectText));
        for (int i = strings.size() - 1; i >= 0; i--) {
            if (strings.get(i) == null) strings.remove(i);
        }

        int index = strings.indexOf(currSelect);
        if (index == -1 || index == strings.size() - 1) {
            currSelect = strings.get(0);
        } else {
            currSelect = strings.get(index + 1);
        }
    }

    private void selectInnerNext() {
        List<String> strings = new ArrayList<>(Arrays.asList(innerSelectKey, innerSelectLevel, innerSelectText));
        for (int i = strings.size() - 1; i >= 0; i--) {
            if (strings.get(i) == null) strings.remove(i);
        }

        int index = strings.indexOf(currSelect);
        if (index == -1 || index == strings.size() - 1) {
            currSelect = strings.get(0);
        } else {
            currSelect = strings.get(index + 1);
        }
    }

    private void setNodeSelectInfo(AccessibilityNodeInfo nodeInfo) {
        String resourceName = nodeInfo.getViewIdResourceName();
        if (resourceName != null && !resourceName.isEmpty()) {
            Pattern pattern = Pattern.compile(".+:(id/.+)");
            Matcher matcher = pattern.matcher(resourceName);
            if (matcher.find() && matcher.group(1) != null) {
                selectKey = matcher.group(1);
            }
        } else selectKey = null;

        CharSequence text = nodeInfo.getText();
        if (text != null && text.length() > 0) {
            selectText = text.toString();
        } else selectText = null;

        selectLevel = getNodeLevel(nodeInfo);
        if (selectLevel != null) selectLevel = "lv/" + selectLevel;
    }

    private void setInnerNodeSelectInfo(AccessibilityNodeInfo nodeInfo) {
        String resourceName = nodeInfo.getViewIdResourceName();
        if (resourceName != null && !resourceName.isEmpty()) {
            Pattern pattern = Pattern.compile(".+:(id/.+)");
            Matcher matcher = pattern.matcher(resourceName);
            if (matcher.find() && matcher.group(1) != null) {
                innerSelectKey = matcher.group(1);
            }
        } else innerSelectKey = null;

        CharSequence text = nodeInfo.getText();
        if (text != null && text.length() > 0) {
            innerSelectText = text.toString();
        } else innerSelectText = null;

        innerSelectLevel = getNodeLevel(nodeInfo);
        if (innerSelectLevel != null) innerSelectLevel = "lv/" + innerSelectLevel;
    }

    private String getNodeLevel(AccessibilityNodeInfo nodeInfo) {
        AccessibilityNodeInfo parent = nodeInfo.getParent();
        if (parent != null) {
            for (int i = 0; i < parent.getChildCount(); i++) {
                AccessibilityNodeInfo child = parent.getChild(i);
                if (child != null && child.equals(nodeInfo)) {
                    String level = getNodeLevel(parent);
                    if (level != null && !level.isEmpty()) {
                        return level + "," + i;
                    } else {
                        return String.valueOf(i);
                    }
                }
            }
        }
        return null;
    }

    @Nullable
    private AccessibilityNodeInfo getNodeIn(int x, int y) {
        if (rootNode == null) return null;
        Map<Integer, AccessibilityNodeInfo> deepNodeInfo = new HashMap<>();
        findNodeIn(deepNodeInfo, 1, rootNode, x, y);
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

    private void findNodeIn(Map<Integer, AccessibilityNodeInfo> deepNodeInfo, int deep, @NonNull AccessibilityNodeInfo nodeInfo, int x, int y) {
        if (nodeInfo.getChildCount() == 0) return;
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null) {
                Rect rect = new Rect();
                child.getBoundsInScreen(rect);
                if (rect.contains(x, y)) {
                    deepNodeInfo.put(deep, child);
                    findNodeIn(deepNodeInfo, deep + 1, child, x, y);
                }
            }
        }
    }

    public AccessibilityNodeInfo getClickableNode(AccessibilityNodeInfo nodeInfo) {
        if (nodeInfo == null) return null;
        if (nodeInfo.isClickable()) return nodeInfo;
        return getClickableNode(nodeInfo.getParent());
    }

    protected class WordPickerCallback extends FloatBaseCallback {
        @Override
        public void onCreate(boolean succeed) {
            super.onCreate(succeed);
            if (succeed) {
                markAll();
                if (textNode != null)
                    showWordView(textNode.searchClickableNode(textNode.searchNodes(rootNode)), true);
            }
        }
    }

}
