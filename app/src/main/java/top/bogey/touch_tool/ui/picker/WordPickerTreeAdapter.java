package top.bogey.touch_tool.ui.picker;

import android.content.res.ColorStateList;
import android.view.View;
import android.view.accessibility.AccessibilityNodeInfo;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.amrdeveloper.treeview.TreeNode;
import com.amrdeveloper.treeview.TreeNodeManager;
import com.amrdeveloper.treeview.TreeViewAdapter;
import com.amrdeveloper.treeview.TreeViewHolder;
import com.amrdeveloper.treeview.TreeViewHolderFactory;

import java.util.ArrayList;
import java.util.Collections;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.utils.DisplayUtils;

public class WordPickerTreeAdapter extends TreeViewAdapter {
    private TreeNode selectedNode;
    private final TreeNodeManager manager;

    public WordPickerTreeAdapter(TreeViewHolderFactory factory, TreeNodeManager manager, WordPickerFloatView picker) {
        super(factory, manager);
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
        TreeNode node = manager.get(position);
        if (node.equals(selectedNode)){
            ViewHolder viewHolder = (ViewHolder) holder;
            viewHolder.titleText.setTextColor(DisplayUtils.getAttrColor(viewHolder.itemView.getContext(), com.google.android.material.R.attr.colorError, 0));
        }
    }

    public void setRoot(AccessibilityNodeInfo root){
        TreeNode tree = createTree(root, 0);
        ArrayList<TreeNode> treeNodes = new ArrayList<>(Collections.singleton(tree));
        updateTreeNodes(treeNodes);
    }

    public void setSelectedNode(TreeNode node){
        selectedNode = node;
    }

    private TreeNode createTree(AccessibilityNodeInfo root, int level){
        TreeNode node = new TreeNode(root, R.layout.float_picker_word_item);
        node.setLevel(level);
        for (int i = 0; i < root.getChildCount(); i++) {
            AccessibilityNodeInfo child = root.getChild(i);
            if (child != null){
                node.addChild(createTree(child, level + 1));
            }
        }
        return node;
    }

    protected static class ViewHolder extends TreeViewHolder{
        public final TextView titleText;
        public final ImageView imageView;
        private final ConstraintLayout layout;
        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            titleText = itemView.findViewById(R.id.title_text);
            imageView = itemView.findViewById(R.id.image_view);
            layout = itemView.findViewById(R.id.constraintLayout);
        }

        @Override
        public void bindTreeNode(TreeNode node) {
            int padding = node.getLevel() * 16;
            layout.setPaddingRelative(padding, 0, 0, 0);
            AccessibilityNodeInfo value = (AccessibilityNodeInfo) node.getValue();
            titleText.setText(getNodeTitle(value));

            int color;
            if (value.isClickable()){
                color = DisplayUtils.getAttrColor(itemView.getContext(), com.google.android.material.R.attr.colorPrimary, 0);
            } else {
                color = DisplayUtils.getAttrColor(itemView.getContext(), com.google.android.material.R.attr.colorOnSurface, 0);
            }
            titleText.setTextColor(color);
            imageView.setImageTintList(ColorStateList.valueOf(color));

            imageView.setVisibility(node.getChildren().size() > 0 ? View.VISIBLE : View.INVISIBLE);
            imageView.setImageResource(node.isExpanded() ? R.drawable.icon_up : R.drawable.icon_down);
        }

        private String getNodeTitle(AccessibilityNodeInfo node){
            StringBuilder builder = new StringBuilder();
            builder.append(node.getClassName());
            CharSequence text = node.getText();
            if (text != null && text.length() > 0){
                builder.append(" | ");
                builder.append(text);
            }

            String resourceName = node.getViewIdResourceName();
            if (resourceName != null && !resourceName.isEmpty()){
                String[] split = resourceName.split(":");
                builder.append(" [ ");
                builder.append(split[1]);
                builder.append(" ]");
            }

            return builder.toString();
        }
    }
}
