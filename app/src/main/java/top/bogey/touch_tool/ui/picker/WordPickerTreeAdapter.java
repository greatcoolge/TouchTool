package top.bogey.touch_tool.ui.picker;

import android.content.Context;
import android.content.res.ColorStateList;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityNodeInfo;

import androidx.annotation.NonNull;

import com.amrdeveloper.treeview.TreeNode;
import com.amrdeveloper.treeview.TreeNodeManager;
import com.amrdeveloper.treeview.TreeViewAdapter;
import com.amrdeveloper.treeview.TreeViewHolder;

import java.util.ArrayList;
import java.util.Collections;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.FloatPickerWordItemBinding;
import top.bogey.touch_tool.utils.DisplayUtils;

public class WordPickerTreeAdapter extends TreeViewAdapter {
    private TreeNode selectedNode;
    private final TreeNodeManager manager;

    public WordPickerTreeAdapter(TreeNodeManager manager, WordPickerFloatView picker) {
        super(null, manager);
        this.manager = manager;
        setTreeNodeLongClickListener((treeNode, view) -> {
            AccessibilityNodeInfo nodeInfo = (AccessibilityNodeInfo) treeNode.getValue();
            picker.showWordView(nodeInfo, false);
            setSelectedNode(treeNode);
            notifyDataSetChanged();
            return true;
        });
    }

    @Override
    public void onBindViewHolder(@NonNull TreeViewHolder holder, int position) {
        super.onBindViewHolder(holder, position);
        ((ViewHolder) holder).refreshItem(manager.get(position), selectedNode);
    }

    @NonNull
    @Override
    public TreeViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int layoutId) {
        return new ViewHolder(FloatPickerWordItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    public void setRoot(AccessibilityNodeInfo root) {
        TreeNode tree = createTree(root, 0);
        ArrayList<TreeNode> treeNodes = new ArrayList<>(Collections.singleton(tree));
        updateTreeNodes(treeNodes);
    }

    public void setSelectedNode(TreeNode node) {
        selectedNode = node;
    }

    private TreeNode createTree(AccessibilityNodeInfo root, int level) {
        TreeNode node = new TreeNode(root, R.layout.float_picker_word_item);
        node.setLevel(level);
        for (int i = 0; i < root.getChildCount(); i++) {
            AccessibilityNodeInfo child = root.getChild(i);
            if (child != null) {
                node.addChild(createTree(child, level + 1));
            }
        }
        return node;
    }

    protected static class ViewHolder extends TreeViewHolder {
        private final FloatPickerWordItemBinding binding;
        private final Context context;

        public ViewHolder(@NonNull FloatPickerWordItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();
        }

        @Override
        public void bindTreeNode(TreeNode node) {
            int padding = node.getLevel() * DisplayUtils.dp2px(context, 8);
            binding.contentBox.setPaddingRelative(padding, 0, 0, 0);
        }

        public void refreshItem(TreeNode node, TreeNode selectedNode) {
            AccessibilityNodeInfo value = (AccessibilityNodeInfo) node.getValue();
            binding.titleText.setText(getNodeTitle(value));

            int color;
            if (value.isClickable()) {
                color = DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorPrimary, 0);
            } else {
                color = DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorOnSurface, 0);
            }
            binding.titleText.setTextColor(color);
            binding.imageView.setImageTintList(ColorStateList.valueOf(color));

            binding.imageView.setVisibility(node.getChildren().size() > 0 ? View.VISIBLE : View.INVISIBLE);
            binding.imageView.setImageResource(node.isExpanded() ? R.drawable.icon_up : R.drawable.icon_down);

            if (node.equals(selectedNode)) {
                binding.titleText.setTextColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorError, 0));
            }
        }

        private String getNodeTitle(AccessibilityNodeInfo node) {
            StringBuilder builder = new StringBuilder();
            builder.append(node.getClassName());
            CharSequence text = node.getText();
            if (text != null && text.length() > 0) {
                builder.append(" | ");
                builder.append(text);
            }

            String resourceName = node.getViewIdResourceName();
            if (resourceName != null && !resourceName.isEmpty()) {
                String[] split = resourceName.split(":");
                if (split.length > 1) {
                    builder.append(" [ ");
                    builder.append(split[1]);
                    builder.append(" ]");
                } else {
                    builder.append(resourceName);
                }
            }

            return builder.toString();
        }
    }
}
