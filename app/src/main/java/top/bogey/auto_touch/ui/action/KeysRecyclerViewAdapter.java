package top.bogey.auto_touch.ui.action;

import android.text.Editable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.databinding.DialogFragmentActionEditItemBinding;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;

public class KeysRecyclerViewAdapter extends RecyclerView.Adapter<KeysRecyclerViewAdapter.ViewHolder> {
    private final ActionEditDialogFragment parent;
    private final List<Node> nodes = new ArrayList<>();

    public KeysRecyclerViewAdapter(ActionEditDialogFragment parent, List<Node> nodes){
        this.parent = parent;
        if (nodes != null) this.nodes.addAll(nodes);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(DialogFragmentActionEditItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        if (position == nodes.size()){
            holder.addLayout.setVisibility(View.VISIBLE);
            holder.infoLayout.setVisibility(View.INVISIBLE);
        } else {
            holder.addLayout.setVisibility(View.INVISIBLE);
            holder.infoLayout.setVisibility(View.VISIBLE);
            Node node = nodes.get(position);
            if (node.type == NodeType.WORD){
                holder.editText.setVisibility(View.VISIBLE);
                holder.iconImage.setVisibility(View.INVISIBLE);
                holder.select.setVisibility(View.INVISIBLE);
                holder.editText.setText(node.getWord());
            } else {
                holder.editText.setVisibility(View.INVISIBLE);
                holder.iconImage.setVisibility(View.VISIBLE);
                holder.select.setVisibility(View.VISIBLE);
                if (node.getImage() != null) holder.iconImage.setImageBitmap(node.getImage());
            }
        }
    }

    @Override
    public int getItemCount() {
        return nodes.size() + 1;
    }

    public List<Node> getNodes(){
        List<Node> realNodes = new ArrayList<>();
        for (Node node : nodes) {
            boolean flag = false;
            switch (node.type) {
                case WORD:
                    flag = node.getWord().isEmpty();
                    break;
                case IMAGE:
                    flag = node.getImage() == null;
                    break;
            }
            if (!flag){
                realNodes.add(node);
            }
        }
        return realNodes;
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final ConstraintLayout infoLayout;
        public final ConstraintLayout addLayout;

        public final Button delete;
        public final EditText editText;
        public final ImageView iconImage;
        public final Button select;

        public final Button textButton;
        public final Button imageButton;


        public ViewHolder(DialogFragmentActionEditItemBinding binding) {
            super(binding.getRoot());
            textButton = binding.targetTextButton;
            imageButton = binding.targetImageButton;

            infoLayout = binding.infoBox;
            addLayout = binding.addBox;

            editText = binding.editText;
            iconImage = binding.iconImage;
            delete = binding.deleteButton;
            select = binding.selectImageButton;

            textButton.setOnClickListener(v -> {
                nodes.add(new Node(NodeType.WORD));
                notifyItemInserted(nodes.size() - 1);
            });

            imageButton.setOnClickListener(v -> {
                nodes.add(new Node(NodeType.IMAGE));
                notifyItemInserted(nodes.size() - 1);
            });

            delete.setOnClickListener(v -> {
                int index = getAdapterPosition();
                nodes.remove(index);
                notifyItemRemoved(index);
            });

            select.setOnClickListener(v -> {

            });

            editText.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)){
                    Editable text = editText.getText();
                    int index = getAdapterPosition();
                    Node node = nodes.get(index);
                    if (text != null && text.length() > 0){
                        node.setWord(text.toString());
                    } else {
                        editText.setText(node.getWord());
                    }
                }
                return true;
            });
        }
    }
}