package top.bogey.touch_tool.ui.actions;

import android.annotation.SuppressLint;
import android.app.ActivityManager;
import android.content.Context;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.FloatActionItemBinding;
import top.bogey.touch_tool.room.bean.Action;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.room.bean.node.ColorNode;
import top.bogey.touch_tool.room.bean.node.DelayNode;
import top.bogey.touch_tool.room.bean.node.ImageNode;
import top.bogey.touch_tool.room.bean.node.KeyNode;
import top.bogey.touch_tool.room.bean.node.Node;
import top.bogey.touch_tool.room.bean.node.NodeType;
import top.bogey.touch_tool.room.bean.node.TaskNode;
import top.bogey.touch_tool.room.bean.node.TextNode;
import top.bogey.touch_tool.room.bean.node.TimeArea;
import top.bogey.touch_tool.room.bean.node.TouchNode;
import top.bogey.touch_tool.room.data.TaskRepository;
import top.bogey.touch_tool.ui.apps.AppInfo;
import top.bogey.touch_tool.ui.picker.AppPickerFloatView;
import top.bogey.touch_tool.ui.picker.ColorPickerFloatView;
import top.bogey.touch_tool.ui.picker.ImagePickerFloatView;
import top.bogey.touch_tool.ui.picker.TouchPickerFloatView;
import top.bogey.touch_tool.ui.picker.WordPickerFloatView;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

public class ActionsRecyclerViewAdapter extends RecyclerView.Adapter<ActionsRecyclerViewAdapter.ViewHolder> {
    private final Task task;
    private final List<Node> nodes = new ArrayList<>();

    private int maxCount = 1;

    public ActionsRecyclerViewAdapter(Task task, List<Node> nodes) {
        this.task = task;
        if (nodes != null) {
            for (Node node : nodes) {
                this.nodes.add(node.clone());
            }
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatActionItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.refreshItem(nodes.get(position));
    }

    @Override
    public int getItemCount() {
        return nodes.size();
    }

    public void addNode(Context context, NodeType type){
        if (nodes.size() < maxCount){
            Node node;
            switch (type) {
                case DELAY:
                    node = new DelayNode(new TimeArea(1000, 1000));
                    break;
                case IMAGE:
                    node = new ImageNode(new ImageNode.ImageInfo(95));
                    break;
                case TOUCH:
                    node = new TouchNode(new TouchNode.TouchPath(context));
                    break;
                case COLOR:
                    node = new ColorNode(new ColorNode.ColorInfo(context));
                    break;
                case KEY:
                    node = new KeyNode(KeyNode.KeyType.BACK);
                    break;
                case TASK:
                    node = new TaskNode(null);
                    break;
                default:
                    node = new TextNode("");
                    break;
            }
            nodes.add(node);
            notifyItemInserted(nodes.size() - 1);
        }
    }

    public void setMaxCount(int maxCount){
        this.maxCount = maxCount;
        while (nodes.size() > maxCount){
            nodes.remove(nodes.size() - 1);
            notifyItemRemoved(nodes.size());
        }
    }

    public List<Node> getNodes(){
        for (int i = nodes.size() - 1; i >= 0; i--) {
            Node node = nodes.get(i);
            TimeArea timeArea = node.getTimeArea();
            if (timeArea.getRealMin() <= 0) {
                nodes.remove(i);
                continue;
            }

            if (!node.isValid()) nodes.remove(i);
        }
        return nodes;
    }

    protected static class TextChangedWatcher implements TextWatcher{
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {

        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    }

    protected class ViewHolder extends RecyclerView.ViewHolder{
        private final FloatActionItemBinding binding;
        private final Context context;
        private final ArrayAdapter<TaskNode.TaskInfo> adapter;

        @SuppressLint("NonConstantResourceId")
        public ViewHolder(FloatActionItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();
            initEditText(itemView);

            binding.timeInclude.setTextWatcher((isMin, editable) -> {
                int index = getBindingAdapterPosition();
                Node node = nodes.get(index);
                if (isMin){
                    if (editable != null && editable.length() > 0){
                        node.getTimeArea().setMin(Integer.parseInt(String.valueOf(editable)));
                    } else {
                        node.getTimeArea().setMin(0);
                    }
                } else {
                    if (editable != null && editable.length() > 0){
                        node.getTimeArea().setMax(Integer.parseInt(String.valueOf(editable)));
                    } else {
                        node.getTimeArea().setMax(0);
                    }
                }
            });

            binding.deleteButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                nodes.remove(index);
                notifyItemRemoved(index);
            });

            binding.delayInclude.setTextWatcher((isMin, editable) -> {
                int index = getBindingAdapterPosition();
                DelayNode node = (DelayNode) nodes.get(index);
                if (isMin){
                    if (editable != null && editable.length() > 0){
                        node.getValue().setMin(Integer.parseInt(String.valueOf(editable)));
                    } else {
                        node.getValue().setMin(0);
                    }
                } else {
                    if (editable != null && editable.length() > 0){
                        node.getValue().setMax(Integer.parseInt(String.valueOf(editable)));
                    } else {
                        node.getValue().setMax(0);
                    }
                }
            });

            binding.textInclude.textBaseInclude.titleEdit.addTextChangedListener(new TextChangedWatcher(){
                @Override
                public void afterTextChanged(Editable s) {
                    int index = getBindingAdapterPosition();
                    Node node = nodes.get(index);
                    if (node.getType() == NodeType.TEXT) node.setValue(String.valueOf(s));
                }
            });

            binding.textInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                TextNode textNode = (TextNode) nodes.get(index);
                new WordPickerFloatView(context, picker -> {
                    WordPickerFloatView wordPicker = (WordPickerFloatView) picker;
                    String word = wordPicker.getWord();
                    textNode.setValue(word);
                    binding.textInclude.textBaseInclude.titleEdit.setText(word);
                }, textNode).show();
            });

            binding.touchInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                TouchNode touchNode = (TouchNode) nodes.get(index);
                new TouchPickerFloatView(context, picker -> {
                    TouchPickerFloatView touchPicker = (TouchPickerFloatView) picker;
                    TouchNode.TouchPath path = touchPicker.getPath();
                    touchNode.setValue(path);
                    binding.touchInclude.touchPath.setPath(path);
                }, touchNode.getValue()).show();
            });

            binding.touchInclude.playButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Node node = nodes.get(index);
                TouchNode touchNode = (TouchNode) node;
                MainAccessibilityService service = MainApplication.getService();
                if (touchNode.isValid()){
                    if (service != null){
                        Task task = new Task();
                        Action action = new Action();
                        action.setTargets(Collections.singletonList(touchNode));
                        task.setActions(Collections.singletonList(action));
                        service.runTask(task, null);
                    } else {
                        Toast.makeText(context, R.string.capture_service_on_tips_3, Toast.LENGTH_SHORT).show();
                    }
                }
            });

            binding.imageInclude.similarText.addTextChangedListener(new TextChangedWatcher(){
                @Override
                public void afterTextChanged(Editable s) {
                    int index = getBindingAdapterPosition();
                    ImageNode node = (ImageNode) nodes.get(index);
                    if (s != null && s.length() > 0){
                        node.getValue().setValue(Integer.parseInt(String.valueOf(s)));
                    } else {
                        node.getValue().setValue(100);
                    }
                }
            });

            binding.imageInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Node node = nodes.get(index);
                ImageNode imageNode = (ImageNode) node;
                new ImagePickerFloatView(context, picker -> {
                    ImagePickerFloatView imagePicker = (ImagePickerFloatView) picker;
                    Bitmap bitmap = imagePicker.getBitmap();
                    imageNode.getValue().setBitmap(bitmap, DisplayUtils.getScreen(context));
                    binding.imageInclude.image.setImageBitmap(bitmap);
                }, imageNode).show();
            });

            binding.colorInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Node node = nodes.get(index);
                ColorNode colorNode = (ColorNode) node;
                new ColorPickerFloatView(context, picker -> {
                    ColorPickerFloatView colorPicker = (ColorPickerFloatView) picker;
                    ColorNode.ColorInfo color = colorPicker.getColor();
                    colorNode.setValue(color);
                    binding.colorInclude.colorCard.setCardBackgroundColor(DisplayUtils.getColorFromHsv(colorNode.getValue().getColor()));
                    binding.colorInclude.similarText.setText(colorNode.getTitle());
                }, colorNode).show();
            });

            adapter = new ArrayAdapter<>(context, R.layout.float_action_spinner_item);
            binding.spinnerInclude.spinner.setAdapter(adapter);
            binding.spinnerInclude.spinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
                @Override
                public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                    int index = getBindingAdapterPosition();
                    Node node = nodes.get(index);
                    TaskNode.TaskInfo taskInfo = adapter.getItem(position);
                    if (node.getType() == NodeType.KEY) {
                        KeyNode keyNode = (KeyNode) node;
                        KeyNode.KeyType keyType = KeyNode.KeyType.valueOf(taskInfo.getId());
                        if (keyType != keyNode.getValue().getKeyType()){
                            keyNode.setValue(new KeyNode.KeyTask(keyType));
                        }

                        if (keyType == KeyNode.KeyType.GOTO){
                            binding.spinnerInclude.pickerButton.setVisibility(View.VISIBLE);
                            binding.spinnerInclude.image.setVisibility(View.VISIBLE);

                            MainViewModel viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
                            if (keyNode.getValue().getExtras() != null){
                                AppInfo info = viewModel.getAppInfoByPkgName(keyNode.getValue().getExtras());
                                if (info != null){
                                    PackageManager manager = context.getPackageManager();
                                    binding.spinnerInclude.image.setImageDrawable(info.info.applicationInfo.loadIcon(manager));
                                }
                            }

                        } else {
                            binding.spinnerInclude.pickerButton.setVisibility(View.GONE);
                            binding.spinnerInclude.image.setVisibility(View.GONE);
                        }
                    } else if (node.getType() == NodeType.TASK){
                        node.setValue(taskInfo);
                    }
                }

                @Override
                public void onNothingSelected(AdapterView<?> parent) {

                }
            });

            binding.spinnerInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Node node = nodes.get(index);
                KeyNode keyNode = (KeyNode) node;
                new AppPickerFloatView(context, picker -> {
                    AppPickerFloatView appPicker = (AppPickerFloatView) picker;
                    AppInfo info = appPicker.getSelectApp();
                    if (info != null){
                        KeyNode.KeyTask value = keyNode.getValue();
                        value.setExtras(info.packageName);

                        PackageManager manager = context.getPackageManager();
                        if (info.packageName.equals(context.getString(R.string.common_package_name))){
                            binding.spinnerInclude.image.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
                        } else {
                            binding.spinnerInclude.image.setImageDrawable(info.info.applicationInfo.loadIcon(manager));
                        }
                    }
                }).show();
            });

            binding.spinnerInclude.image.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Node node = nodes.get(index);
                KeyNode keyNode = (KeyNode) node;
                String extras = keyNode.getValue().getExtras();
                if (extras == null || extras .isEmpty()) return;
                AppUtils.gotoApp(context, extras);
            });

            binding.upButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.max(0, index - 1);
                nodes.add(newIndex, nodes.remove(index));
                notifyItemRangeChanged(newIndex, 2);
            });

            binding.downButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.min(nodes.size() - 1, index + 1);
                nodes.add(newIndex, nodes.remove(index));
                notifyItemRangeChanged(index, 2);
            });
        }

        public void refreshItem(Node node){
            String[] strings = context.getResources().getStringArray(R.array.node_type);
            binding.titleText.setText(strings[node.getType().ordinal()]);

            binding.timeInclude.setValue(node.getTimeArea().getMin(), node.getTimeArea().getMax());

            switch (node.getType()) {
                case DELAY:
                    binding.delayInclude.setVisibility(View.VISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.GONE);
                    binding.delayInclude.setValue(((DelayNode) node).getValue().getMin(), ((DelayNode) node).getValue().getMax());
                    break;
                case COLOR:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.timeInclude.setVisibility(View.VISIBLE);
                    binding.colorInclude.colorCard.setCardBackgroundColor(DisplayUtils.getColorFromHsv(((ColorNode) node).getValue().getColor()));
                    binding.colorInclude.similarText.setText(((ColorNode) node).getTitle());
                    break;
                case TEXT:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.VISIBLE);
                    binding.textInclude.pickerButton.setIconResource(R.drawable.icon_text);
                    binding.textInclude.textBaseInclude.textInputLayout.setEnabled(true);
                    binding.textInclude.textBaseInclude.titleEdit.setText(((TextNode) node).getValue());
                    break;
                case TOUCH:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.VISIBLE);
                    binding.touchInclude.touchPath.setPath(((TouchNode) node).getValue());
                    break;
                case IMAGE:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.VISIBLE);
                    ImageNode.ImageInfo imageInfo = ((ImageNode) node).getValue();
                    binding.imageInclude.similarText.setText(String.valueOf(imageInfo.getValue()));
                    if (imageInfo.getBitmap() != null){
                        binding.imageInclude.image.setImageBitmap(imageInfo.getBitmap());
                    }
                    break;
                case KEY:
                case TASK:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.GONE);
                    if (node.getType() == NodeType.KEY){
                        adapter.clear();
                        for (KeyNode.KeyType keyType : KeyNode.KeyType.values()) {
                            adapter.add(new TaskNode.TaskInfo(keyType.name(), keyType.getTitle(context, "")));
                        }
                        selectSpinner(((KeyNode) node).getValue().getKeyType().name());
                    } else {
                        adapter.clear();
                        for (Task taskItem : TaskRepository.getInstance(context).getAllTasks()) {
                            if (taskItem.getStatus() == TaskStatus.CLOSED
                                    && !taskItem.getId().equals(task.getId())
                                    && (taskItem.getPkgName().equals(task.getPkgName()) || taskItem.getPkgName().equals(context.getString(R.string.common_package_name)))){
                                adapter.add(new TaskNode.TaskInfo(taskItem.getId(), taskItem.getTitle()));
                            }
                        }

                        TaskNode.TaskInfo taskInfo = ((TaskNode) node).getValue();
                        if (taskInfo == null){
                            selectSpinner("");
                        } else {
                            selectSpinner(taskInfo.getId());
                        }
                    }
                    break;
            }
        }

        private void initEditText(View view){
            if (view instanceof ViewGroup){
                ViewGroup viewGroup = (ViewGroup) view;
                for (int i = 0; i < viewGroup.getChildCount(); i++) {
                    initEditText(viewGroup.getChildAt(i));
                }
            } else {
                if (view instanceof EditText){
                    EasyFloat.initInput((EditText) view, ActionFloatView.class.getCanonicalName());
                }
            }
        }

        private void selectSpinner(String id){
            Spinner spinner = binding.spinnerInclude.spinner;
            for (int i = 0; i < adapter.getCount(); i++) {
                TaskNode.TaskInfo item = adapter.getItem(i);
                if (item.getId().equals(id)) {
                    spinner.setSelection(i);
                    return;
                }
            }
            if (adapter.getCount() > 0){
                spinner.setSelection(0);
                AdapterView.OnItemSelectedListener listener = spinner.getOnItemSelectedListener();
                listener.onItemSelected(spinner, spinner.getSelectedView(), 0, adapter.getItemId(0));
            }
        }
    }
}
