package top.bogey.touch_tool.ui.picker;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.amrdeveloper.treeview.TreeNode;
import com.amrdeveloper.treeview.TreeNodeManager;
import com.amrdeveloper.treeview.TreeViewHolderFactory;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.FloatPickerWordBinding;
import top.bogey.touch_tool.room.bean.node.TextNode;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.FloatClickCallback;

@SuppressLint("ViewConstructor")
public class WordPickerFloatView extends BasePickerFloatView{
    private final FloatPickerWordBinding binding;
    private final TextNode textNode;
    private boolean addEnabled = true;

    private TreeNodeManager manager;
    private boolean showList = false;

    private AccessibilityNodeInfo rootNode;
    private AccessibilityNodeInfo selectNode = null;

    private String selectKey = "";
    private String selectLevel = "";

    public WordPickerFloatView(@NonNull Context context, PickerCallback pickerCallback, TextNode textNode) {
        super(context, pickerCallback);
        this.textNode = textNode;

        binding = FloatPickerWordBinding.inflate(LayoutInflater.from(context), this, true);

        floatCallback = new WordPickerClickCallback((rawX, rawY) -> binding.getRoot().postDelayed(() -> {
            if (addEnabled && !showList){
                AccessibilityNodeInfo node = getClickableNodeIn(rawX, rawY);
                if (node != null){
                    showWordView(node, true);
                }
            }
            addEnabled = true;
        }, 50));

        binding.saveButton.setOnClickListener(v -> {
            addEnabled = false;
            if (pickerCallback != null){
                pickerCallback.onComplete(this);
            }
            dismiss();
        });

        binding.switchButton.setOnClickListener(v -> {
            addEnabled = false;
            ViewGroup.LayoutParams params = binding.constraintLayout.getLayoutParams();
            int px = DisplayUtils.dp2px(context, 300);
            int start = showList ? px : 0;
            showList = !showList;
            binding.switchButton.setText(showList ? R.string.word_picker_tips : R.string.word_picker_open);
            ValueAnimator animator = ValueAnimator.ofInt(start, px - start);
            animator.addUpdateListener(animation -> {
                params.height = (int) animation.getAnimatedValue();
                binding.constraintLayout.setLayoutParams(params);
            });
            animator.addListener(new AnimatorListenerAdapter() {
                @Override
                public void onAnimationEnd(Animator animation) {
                    super.onAnimationEnd(animation);
                    if (!showList) binding.wordRecyclerView.setVisibility(View.GONE);
                }

                @Override
                public void onAnimationStart(Animator animation) {
                    super.onAnimationStart(animation);
                    binding.wordRecyclerView.postDelayed(() -> {
                        if (showList) binding.wordRecyclerView.setVisibility(View.VISIBLE);
                    }, 50);
                }
            });
            animator.start();
        });

        binding.titleText.setOnClickListener(v -> {
            addEnabled = false;
            CharSequence text = binding.titleText.getText();
            if (text != null && text.length() > 0){
                if (text.toString().equals(selectKey)){
                    binding.titleText.setText(selectLevel);
                } else {
                    binding.titleText.setText(selectKey);
                }
            } else {
                binding.titleText.setText(selectKey);
            }
        });

        binding.markBox.setOnClickListener(v -> {
            addEnabled = false;
            showWordView(null, false);
        });
    }

    public String getWord(){
        if (selectNode != null){
            CharSequence text = binding.titleText.getText();
            if (text != null && text.length() > 0) return String.format("\"%s\"", text);
        }
        return "";
    }

    private void markAll(){
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
            rootNode = service.getRootInActiveWindow();
            TreeViewHolderFactory factory = (view, layoutId) -> new WordPickerTreeAdapter.ViewHolder(view);
            manager = new TreeNodeManager();
            WordPickerTreeAdapter adapter = new WordPickerTreeAdapter(factory, manager, this);
            binding.wordRecyclerView.setAdapter(adapter);
            adapter.setRoot(rootNode);
        }
    }

    public void showWordView(AccessibilityNodeInfo nodeInfo, boolean selectTreeNode){
        if (nodeInfo == null){
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

            WordPickerTreeAdapter adapter = (WordPickerTreeAdapter) binding.wordRecyclerView.getAdapter();
            if (selectTreeNode && adapter != null){
                adapter.collapseAll();
                if (manager.size() > 0){
                    TreeNode treeNode = manager.get(0);
                    TreeNode node = findTreeNode(treeNode, nodeInfo);
                    if (node != null){
                        node.setSelected(true);
                        adapter.setSelectedNode(node);
                        TreeNode parent = node.getParent();
                        while (parent != null){
                            TreeNode p = parent.getParent();
                            if (p != null){
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
    }

    private TreeNode findTreeNode(TreeNode treeNode, Object value){
        if (value.equals(treeNode.getValue())) return treeNode;
        for (TreeNode child : treeNode.getChildren()) {
            TreeNode node = findTreeNode(child, value);
            if (node != null) return node;
        }
        return null;
    }

    private String getNodeText(AccessibilityNodeInfo nodeInfo){
        String name = "";
        if (nodeInfo != null){
            String resourceName = nodeInfo.getViewIdResourceName();
            if (resourceName != null && !resourceName.isEmpty()){
                name = resourceName;
            }
            CharSequence text = nodeInfo.getText();
            if (name.isEmpty() && text != null && text.length() > 0){
                name = text.toString();
            }
        }
        return name;
    }

    private String getNodeKey(AccessibilityNodeInfo nodeInfo){
        String key = getNodeText(nodeInfo);
        Pattern pattern = Pattern.compile(".+:(id/.+)");
        Matcher matcher = pattern.matcher(key);
        if (matcher.find() && matcher.group(1) != null){
            key = matcher.group(1);
        }
        return key;
    }

    private String getNodeLevel(AccessibilityNodeInfo nodeInfo){
        AccessibilityNodeInfo parent = nodeInfo.getParent();
        if (parent != null){
            for (int i = 0; i < parent.getChildCount(); i++) {
                AccessibilityNodeInfo child = parent.getChild(i);
                if (child != null && child.equals(nodeInfo)){
                    String level = getNodeLevel(parent);
                    if (!level.isEmpty()){
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
    private AccessibilityNodeInfo getClickableNodeIn(int x, int y){
        if (rootNode == null) return null;
        Map<Integer, AccessibilityNodeInfo> deepNodeInfo = new HashMap<>();
        findClickableNodeIn(deepNodeInfo, 1, rootNode, x, y);
        int max = 0;
        AccessibilityNodeInfo node = null;
        for (Map.Entry<Integer, AccessibilityNodeInfo> entry : deepNodeInfo.entrySet()) {
            if (max == 0 || entry.getKey() > max){
                max = entry.getKey();
                node = entry.getValue();
            }
        }
        return node;
    }

    private void findClickableNodeIn(Map<Integer, AccessibilityNodeInfo> deepNodeInfo, int deep, @NonNull AccessibilityNodeInfo nodeInfo, int x, int y){
        if (nodeInfo.getChildCount() == 0) return;
        for (int i = 0; i < nodeInfo.getChildCount(); i++) {
            AccessibilityNodeInfo child = nodeInfo.getChild(i);
            if (child != null){
                Rect rect = new Rect();
                child.getBoundsInScreen(rect);
                if (rect.contains(x, y)){
                    if (child.isClickable()){
                        deepNodeInfo.put(deep, child);
                    }
                    findClickableNodeIn(deepNodeInfo, deep + 1, child, x, y);
                }
            }
        }
    }

    protected class WordPickerClickCallback extends FloatClickCallback{
        public WordPickerClickCallback(ClickCallback callback) {
            super(callback);
        }

        @Override
        public void onCreate(boolean succeed) {
            super.onCreate(succeed);
            if (succeed){
                markAll();
                showWordView(textNode.searchClickableNode(textNode.searchNodes(rootNode)), true);
            }
        }
    }

}
