package top.bogey.auto_touch.ui.action;

import android.graphics.Bitmap;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.databinding.FloatFragmentActionEditItemBinding;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.ui.picker.ImagePicker;

public class KeysRecyclerViewAdapter extends RecyclerView.Adapter<KeysRecyclerViewAdapter.ViewHolder> {
    private final FloatActionEdit parent;
    private final List<Node> nodes = new ArrayList<>();

    public KeysRecyclerViewAdapter(FloatActionEdit parent, List<Node> nodes){
        this.parent = parent;
        if (nodes != null) this.nodes.addAll(nodes);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatFragmentActionEditItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
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

    @Override
    public int getItemCount() {
        return nodes.size();
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

    public void addNewNode(NodeType nodeType){
        nodes.add(new Node(nodeType));
        notifyItemInserted(nodes.size() - 1);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final Button delete;
        public final EditText editText;
        public final ImageView iconImage;
        public final Button select;


        public ViewHolder(FloatFragmentActionEditItemBinding binding) {
            super(binding.getRoot());
            editText = binding.editText;
            iconImage = binding.iconImage;
            delete = binding.deleteButton;
            select = binding.selectImageButton;

            delete.setOnClickListener(v -> {
                int index = getAdapterPosition();
                nodes.remove(index);
                notifyItemRemoved(index);
            });

            select.setOnClickListener(v -> new ImagePicker(parent.getContext(), nodePicker -> {
                ImagePicker imagePicker = (ImagePicker) nodePicker;
                Bitmap bitmap = imagePicker.getBitmap();
                iconImage.setImageBitmap(bitmap);
                int index = getAdapterPosition();
                Node node = nodes.get(index);
                node.setImage(bitmap);
            }).show(Gravity.START | Gravity.TOP, 0, 0));

            editText.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int count, int after) {

                }

                @Override
                public void onTextChanged(CharSequence s, int start, int before, int count) {

                }

                @Override
                public void afterTextChanged(Editable s) {
                    Editable text = editText.getText();
                    int index = getAdapterPosition();
                    Node node = nodes.get(index);
                    if (text != null && text.length() > 0){
                        node.setWord(text.toString());
                    } else {
                        node.setWord("");
                    }
                }
            });
        }
    }
}