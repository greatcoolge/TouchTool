package top.bogey.auto_touch.ui.picker;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.content.Context;
import android.graphics.Point;
import android.graphics.Rect;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.amrdeveloper.treeview.TreeNode;
import com.amrdeveloper.treeview.TreeNodeManager;
import com.amrdeveloper.treeview.TreeViewHolderFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentPickerWordBgBinding;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.util.AppUtil;

public class WordPicker extends NodePicker{
    private final FloatFragmentPickerWordBgBinding binding;
    private boolean addEnabled = true;
    private WordPickerView wordPickerView;
    private String key = "";
    private TreeNodeManager manager;
    private boolean showList = false;
    private AccessibilityNodeInfo rootNode;

    public WordPicker(@NonNull Context context, PickerCallback pickerCallback) {
        super(context, null, pickerCallback);
        binding = FloatFragmentPickerWordBgBinding.inflate(LayoutInflater.from(context));
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

        binding.switchButton.setOnClickListener(v -> {
            addEnabled = false;
            ViewGroup.LayoutParams params = binding.constraintLayout.getLayoutParams();
            int px = AppUtil.dp2px(context, 300);
            int start = showList ? px : 0;
            showList = !showList;
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
                    if (showList) binding.wordRecyclerView.setVisibility(View.VISIBLE);
                }
            });
            animator.start();
        });

        floatCallback = new TouchCallback();
    }

    public String getKey(){
        if (key.isEmpty()) return key;
        return String.format("\"%s\"", key);
    }

    public void addWordView(AccessibilityNodeInfo nodeInfo, boolean selectTreeNode){
        if (nodeInfo != null){
            String key = getNodeText(nodeInfo);
            Rect rect = new Rect();
            nodeInfo.getBoundsInScreen(rect);
            if (wordPickerView != null) wordPickerView.dismiss();
            wordPickerView = new WordPickerView(context, nodePicker -> wordPickerView = null);
            int statusBarHeight = AppUtil.getStatusBarHeight(layout, EasyFloat.getParams(AppUtil.getIdentityCode(WordPicker.this)));
            rect.top = Math.max(0, rect.top - statusBarHeight);
            rect.bottom -= statusBarHeight;
            wordPickerView.show(rect, key);

            if (selectTreeNode){
                WordPickerTreeAdapter adapter = (WordPickerTreeAdapter) binding.wordRecyclerView.getAdapter();
                if (adapter != null) {
                    adapter.collapseAll();
                    if (manager.size() > 0){
                        TreeNode treeNode = manager.get(0);
                        TreeNode node = findTreeNode(treeNode, nodeInfo);
                        if (node != null){
                            expandedToNode(node);
                            adapter.setSelectedNode(node);
                            adapter.notifyDataSetChanged();
                        }
                    }
                }
            }
        }
    }

    private void expandedToNode(TreeNode treeNode){
        List<TreeNode> nodes = new LinkedList<>();
        TreeNode parent = treeNode.getParent();
        while (parent != null){
            nodes.add(0, parent);
            parent = parent.getParent();
        }
        List<TreeNode> showNodes = new LinkedList<>();
        showNodes.add(nodes.get(0));
        for (TreeNode node : nodes) {
            node.setExpanded(true);
            int pos = showNodes.indexOf(node);
            showNodes.addAll(pos + 1, node.getChildren());
        }
        manager.updateNodes(showNodes);
    }

    private TreeNode findTreeNode(TreeNode treeNode, Object value){
        if (value.equals(treeNode.getValue())) return treeNode;
        for (TreeNode child : treeNode.getChildren()) {
            TreeNode node = findTreeNode(child, value);
            if (node != null) return node;
        }
        return null;
    }

    private void markAll(){
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
            rootNode = service.getRootInActiveWindow();
            TreeViewHolderFactory factory = (view, layoutId) -> new WordPickerTreeAdapter.ViewHolder(view);
            manager = new TreeNodeManager();
            WordPickerTreeAdapter adapter = new WordPickerTreeAdapter(factory, manager, this, rootNode);
            binding.wordRecyclerView.setAdapter(adapter);
        }
    }

    private String getNodeText(AccessibilityNodeInfo nodeInfo){
        String name = "";
        if (nodeInfo != null){
            String resourceName = nodeInfo.getViewIdResourceName();
            if (resourceName != null && !resourceName.isEmpty()){
                name = resourceName;
            } else {
                CharSequence text = nodeInfo.getText();
                if (text != null && text.length() > 0){
                    name = text.toString();
                }
            }
        }
        return name;
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

    private class TouchCallback extends FloatPickerShowCallback {
        private boolean drag;
        private float lastX, lastY;

        @Override
        public void onTouch(MotionEvent motionEvent) {
            super.onTouch(motionEvent);
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
                            if (addEnabled && !showList){
                                AccessibilityNodeInfo node = getClickableNodeIn((int) rawX, (int) rawY);
                                if (node != null){
                                    addWordView(node, true);
                                }
                            }
                            addEnabled = true;
                        }, 50);
                    }
            }
        }

        @Override
        public void onDismiss() {
            super.onDismiss();
            if (wordPickerView != null){
                wordPickerView.dismiss();
            }
        }

        @Override
        public void onCreate(boolean succeed) {
            super.onCreate(succeed);
            if (succeed){
                markAll();
            }
        }
    }
}
