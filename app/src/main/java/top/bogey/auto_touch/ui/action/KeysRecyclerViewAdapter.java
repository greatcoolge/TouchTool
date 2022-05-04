package top.bogey.auto_touch.ui.action;

import android.content.Context;
import android.graphics.Bitmap;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.RadioGroup;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentActionEditItemBinding;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.room.bean.SimpleTaskInfo;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.picker.ImagePicker;
import top.bogey.auto_touch.ui.picker.PosPicker;
import top.bogey.auto_touch.ui.picker.WordPicker;

public class KeysRecyclerViewAdapter extends RecyclerView.Adapter<KeysRecyclerViewAdapter.ViewHolder> {
    private final FloatActionEdit parent;
    private final List<Node> nodes = new ArrayList<>();
    private int maxCount = 1;
    private final List<Task> tasks;

    public KeysRecyclerViewAdapter(FloatActionEdit parent, List<Node> nodes){
        this.parent = parent;
        if (nodes != null) this.nodes.addAll(nodes);
        else this.nodes.add(new Node(NodeType.TEXT));
        tasks = parent.viewModel.getTasksByPackageName(parent.task.getPkgName());
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatFragmentActionEditItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        Node node = nodes.get(position);
        switch (node.getType()) {
            case DELAY:
                holder.targetPicker.setVisibility(View.GONE);
                holder.targetEdit.setVisibility(View.VISIBLE);
                holder.targetImage.setVisibility(View.GONE);
                holder.targetSpinner.setVisibility(View.GONE);
                holder.timeGroup.setVisibility(View.VISIBLE);
                holder.pressTime.setVisibility(View.GONE);
                holder.targetEdit.setInputType(EditorInfo.TYPE_CLASS_NUMBER);
                holder.targetEdit.setEnabled(true);
                holder.timeGroup.check(holder.timeGroup.getChildAt(node.getDelay() % 1000 == 0 ? 1 : 0).getId());
                holder.targetEdit.setText(String.valueOf(node.getDelay() / holder.delayScale));
                break;
            case TEXT:
                holder.targetPicker.setVisibility(View.VISIBLE);
                holder.targetEdit.setVisibility(View.VISIBLE);
                holder.targetImage.setVisibility(View.GONE);
                holder.targetSpinner.setVisibility(View.GONE);
                holder.timeGroup.setVisibility(View.GONE);
                holder.pressTime.setVisibility(View.VISIBLE);
                holder.targetPicker.setIconResource(R.drawable.text);
                holder.targetEdit.setInputType(EditorInfo.TYPE_CLASS_TEXT);
                holder.targetEdit.setEnabled(true);
                holder.targetEdit.setText(node.getText());
                break;
            case IMAGE:
                holder.targetPicker.setVisibility(View.VISIBLE);
                holder.targetEdit.setVisibility(View.GONE);
                holder.targetImage.setVisibility(View.VISIBLE);
                holder.targetSpinner.setVisibility(View.GONE);
                holder.timeGroup.setVisibility(View.GONE);
                holder.pressTime.setVisibility(View.VISIBLE);
                holder.targetPicker.setIconResource(R.drawable.image);
                holder.targetImage.setImageBitmap(node.getImage());
                break;
            case POS:
                holder.targetPicker.setVisibility(View.VISIBLE);
                holder.targetEdit.setVisibility(View.VISIBLE);
                holder.targetImage.setVisibility(View.GONE);
                holder.targetSpinner.setVisibility(View.GONE);
                holder.timeGroup.setVisibility(View.GONE);
                holder.pressTime.setVisibility(View.VISIBLE);
                holder.targetPicker.setIconResource(R.drawable.pos);
                holder.targetEdit.setInputType(EditorInfo.TYPE_CLASS_TEXT);
                holder.targetEdit.setEnabled(false);
                holder.targetEdit.setText(node.getText());
                break;
            case KEY:
            case TASK:
                holder.targetPicker.setVisibility(View.GONE);
                holder.targetEdit.setVisibility(View.GONE);
                holder.targetImage.setVisibility(View.GONE);
                holder.targetSpinner.setVisibility(View.VISIBLE);
                holder.timeGroup.setVisibility(View.GONE);
                break;
        }
        switch (node.getType()) {
            case KEY:
                holder.pressTime.setVisibility(View.VISIBLE);
                String[] strings = parent.getContext().getResources().getStringArray(R.array.keys);
                holder.adapter.clear();
                for (int i = 0; i < strings.length; i++) {
                    holder.adapter.add(new SimpleTaskInfo(String.valueOf(i), strings[i]));
                }
                parent.selectSpinner(holder.targetSpinner, node.getText());
                break;
            case TASK:
                holder.pressTime.setVisibility(View.GONE);
                holder.adapter.clear();
                for (Task task : tasks) {
                    if (!task.getId().equals(parent.task.getId())) {
                        holder.adapter.add(new SimpleTaskInfo(task.getId(), task.getTitle()));
                    }
                }
                SimpleTaskInfo task = node.getTask();
                if (task != null) {
                    parent.selectSpinner(holder.targetSpinner, task.getId());
                } else {
                    parent.selectSpinner(holder.targetSpinner, "");
                }
                break;
        }
        String[] strings = parent.getContext().getResources().getStringArray(R.array.node_type);
        holder.titleText.setText(strings[node.getType().ordinal()]);
        holder.pressTime.setText(String.valueOf(node.getTime()));
    }

    @Override
    public int getItemCount() {
        return nodes.size();
    }

    public List<Node> getNodes(){
        List<Node> realNodes = new ArrayList<>();
        for (Node node : nodes) {
            boolean flag = false;
            switch (node.getType()) {
                case TEXT:
                    flag = node.getText().isEmpty();
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

    public void addNewNode(Node node){
        if (nodes.size() < maxCount){
            nodes.add(node);
            notifyItemInserted(nodes.size() - 1);
        } else {
            Toast.makeText(parent.getContext(), R.string.too_much_target, Toast.LENGTH_LONG).show();
        }
    }

    public void setMaxCount(int maxCount){
        this.maxCount = maxCount;
        while (nodes.size() > maxCount){
            nodes.remove(nodes.size() - 1);
            notifyItemRemoved(nodes.size());
        }
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final EditText targetEdit;
        public final ImageView targetImage;
        public final Spinner targetSpinner;
        public final MaterialButton targetPicker;
        public final MaterialButton deleteButton;
        public final RadioGroup timeGroup;
        public final EditText pressTime;
        public final TextView titleText;

        public ArrayAdapter<SimpleTaskInfo> adapter;
        public int delayScale = 1;


        public ViewHolder(FloatFragmentActionEditItemBinding binding) {
            super(binding.getRoot());
            targetEdit = binding.targetEdit;
            targetImage = binding.targetImage;
            targetSpinner = binding.targetSpinner;
            targetPicker = binding.targetPicker;
            deleteButton = binding.deleteButton;
            timeGroup = binding.timeGroup;
            pressTime = binding.pressTime;
            titleText = binding.titleText;

            adapter = new ArrayAdapter<>(parent.getContext(), R.layout.float_fragment_action_edit_picker);
            targetSpinner.setAdapter(adapter);

            targetEdit.setOnFocusChangeListener((v, hasFocus) -> {
                if (hasFocus){
                    InputMethodManager manager = (InputMethodManager) parent.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
                    manager.showSoftInput(targetEdit, 0);
                }
            });

            targetEdit.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int count, int after) {

                }

                @Override
                public void onTextChanged(CharSequence s, int start, int before, int count) {

                }

                @Override
                public void afterTextChanged(Editable s) {
                    Editable text = targetEdit.getText();
                    int index = getAdapterPosition();
                    Node node = nodes.get(index);
                    String value = "";
                    if (text != null && text.length() > 0){
                        value = String.valueOf(text);
                    }
                    switch (node.getType()) {
                        case TEXT:
                            node.setText(value);
                            break;
                        case DELAY:
                            int delay = 0;
                            try {
                                delay = Integer.parseInt(value);
                            } catch (NumberFormatException ignored) {
                            }
                            node.setDelay(delay * delayScale);
                            break;
                    }
                }
            });

            pressTime.setOnFocusChangeListener((v, hasFocus) -> {
                if (hasFocus){
                    InputMethodManager manager = (InputMethodManager) parent.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
                    manager.showSoftInput(pressTime, InputMethodManager.SHOW_FORCED);
                }
            });

            pressTime.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int count, int after) {

                }

                @Override
                public void onTextChanged(CharSequence s, int start, int before, int count) {

                }

                @Override
                public void afterTextChanged(Editable s) {
                    Editable text = pressTime.getText();
                    int index = getAdapterPosition();
                    Node node = nodes.get(index);
                    String value = "";
                    if (text != null && text.length() > 0){
                        value = String.valueOf(text);
                    }
                    if (!value.isEmpty()){
                        node.setTime(Integer.parseInt(value));
                    }
                }
            });

            targetSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
                @Override
                public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                    int index = getAdapterPosition();
                    Node node = nodes.get(index);
                    SimpleTaskInfo item = adapter.getItem(position);
                    switch (node.getType()) {
                        case KEY:
                            node.setKey(Integer.parseInt(item.getId()));
                            break;
                        case TASK:
                            node.setTask(item);
                            break;
                    }
                }

                @Override
                public void onNothingSelected(AdapterView<?> parent) {

                }
            });

            targetPicker.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Node node = nodes.get(index);
                switch (node.getType()) {
                    case TEXT:
                        new WordPicker(parent.getContext(), nodePicker -> {
                            WordPicker wordPicker = (WordPicker) nodePicker;
                            String key = wordPicker.getKey();
                            node.setText(key);
                            targetEdit.setText(key);
                        }).show(0, 0);
                        break;
                    case IMAGE:
                        new ImagePicker(parent.getContext(), nodePicker -> {
                            ImagePicker imagePicker = (ImagePicker) nodePicker;
                            Bitmap bitmap = imagePicker.getBitmap();
                            node.setImage(bitmap);
                            targetImage.setImageBitmap(bitmap);
                        }).show(0, 0);
                        break;
                    case POS:
                        new PosPicker(parent.getContext(), nodePicker -> {
                            PosPicker posPicker = (PosPicker) nodePicker;
                            List<Pos> posList = posPicker.getPosList();
                            node.setPoses(posList);
                            targetEdit.setText(node.getText());
                        }, node.getPoses()).show(0, 0);
                        break;
                }
            });

            deleteButton.setOnClickListener(v -> {
                int index = getAdapterPosition();
                nodes.remove(index);
                notifyItemRemoved(index);
            });

            timeGroup.setOnCheckedChangeListener((group, checkedId) -> {
                int checkIndex = group.indexOfChild(group.findViewById(checkedId));
                delayScale = checkIndex == 0 ? 1 : 1000;

                Editable text = targetEdit.getText();
                if (text != null && text.length() > 0){
                    int delay = Integer.parseInt(text.toString());
                    int index = getAdapterPosition();
                    Node node = nodes.get(index);
                    node.setDelay(delay * delayScale);
                }
            });
        }
    }
}