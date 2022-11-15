package top.bogey.touch_tool.ui.action;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.text.Editable;
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
import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.database.bean.action.ActionType;
import top.bogey.touch_tool.database.bean.action.ColorAction;
import top.bogey.touch_tool.database.bean.action.DelayAction;
import top.bogey.touch_tool.database.bean.action.ImageAction;
import top.bogey.touch_tool.database.bean.action.InputAction;
import top.bogey.touch_tool.database.bean.action.SystemAction;
import top.bogey.touch_tool.database.bean.action.TaskAction;
import top.bogey.touch_tool.database.bean.action.TextAction;
import top.bogey.touch_tool.database.bean.action.TouchAction;
import top.bogey.touch_tool.databinding.FloatActionItemBinding;
import top.bogey.touch_tool.ui.app.AppInfo;
import top.bogey.touch_tool.ui.behavior.BehaviorFloatView;
import top.bogey.touch_tool.ui.picker.AppPickerFloatView;
import top.bogey.touch_tool.ui.picker.ColorPickerFloatView;
import top.bogey.touch_tool.ui.picker.ImagePickerFloatView;
import top.bogey.touch_tool.ui.picker.InputPickerFloatView;
import top.bogey.touch_tool.ui.picker.TouchPickerFloatView;
import top.bogey.touch_tool.ui.picker.WordPickerFloatView;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.TextChangedListener;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

public class ActionsRecyclerViewAdapter extends RecyclerView.Adapter<ActionsRecyclerViewAdapter.ViewHolder> {
    private final Task baseTask;
    private final Task currTask;
    private final List<Action> actions = new ArrayList<>();

    private int maxCount = 1;

    public ActionsRecyclerViewAdapter(Task baseTask, Task currTask, List<Action> actions) {
        this.baseTask = baseTask;
        this.currTask = currTask;
        if (actions != null) {
            this.actions.addAll(actions);
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatActionItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.refreshItem(actions.get(position));
    }

    @Override
    public int getItemCount() {
        return actions.size();
    }

    public void addNode(Action action) {
        if (actions.size() < maxCount) {
            actions.add(action);
            notifyItemInserted(actions.size() - 1);
        }
    }

    public void setMaxCount(int maxCount) {
        this.maxCount = maxCount;
        while (actions.size() > maxCount) {
            actions.remove(actions.size() - 1);
            notifyItemRemoved(actions.size());
        }
    }

    public List<Action> getActions() {
        List<Action> actionList = new ArrayList<>();
        for (Action action : actions) {
            if (action.getTimeArea().getMax() <= 0) {
                continue;
            }
            if (action.isValid()) actionList.add(action);
        }
        return actionList;
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final FloatActionItemBinding binding;
        private final Context context;
        private final ArrayAdapter<TaskAction> adapter;

        @SuppressLint("NonConstantResourceId")
        public ViewHolder(FloatActionItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();
            initEditText(itemView);

            binding.timeInclude.setTextWatcher((isMin, editable) -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                if (isMin) {
                    if (editable != null && editable.length() > 0) {
                        action.getTimeArea().setMin(Integer.parseInt(String.valueOf(editable)));
                    } else {
                        action.getTimeArea().setMin(0);
                    }
                } else {
                    if (editable != null && editable.length() > 0) {
                        action.getTimeArea().setMax(Integer.parseInt(String.valueOf(editable)));
                    } else {
                        action.getTimeArea().setMax(0);
                    }
                }
            });

            binding.deleteButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                actions.remove(index);
                notifyItemRemoved(index);
            });

            binding.delayInclude.setTextWatcher((isMin, editable) -> {
                int index = getBindingAdapterPosition();
                DelayAction delayAction = (DelayAction) actions.get(index);
                if (isMin) {
                    if (editable != null && editable.length() > 0) {
                        delayAction.getTimeArea().setMin(Integer.parseInt(String.valueOf(editable)));
                    } else {
                        delayAction.getTimeArea().setMin(0);
                    }
                } else {
                    if (editable != null && editable.length() > 0) {
                        delayAction.getTimeArea().setMax(Integer.parseInt(String.valueOf(editable)));
                    } else {
                        delayAction.getTimeArea().setMax(0);
                    }
                }
            });

            binding.textInclude.textBaseInclude.titleEdit.addTextChangedListener(new TextChangedListener() {
                @Override
                public void afterTextChanged(Editable s) {
                    int index = getBindingAdapterPosition();
                    Action action = actions.get(index);
                    if (action.getType() == ActionType.TEXT) ((TextAction) action).setText(String.valueOf(s));
                }
            });

            binding.textInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                TextAction textAction = (TextAction) actions.get(index);
                new WordPickerFloatView(context, picker -> {
                    WordPickerFloatView wordPicker = (WordPickerFloatView) picker;
                    String word = wordPicker.getWord();
                    textAction.setText(word);
                    binding.textInclude.textBaseInclude.titleEdit.setText(word);
                }, textAction).show();
            });

            binding.touchInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                TouchAction touchAction = (TouchAction) actions.get(index);
                new TouchPickerFloatView(context, picker -> {
                    TouchPickerFloatView touchPicker = (TouchPickerFloatView) picker;
                    TouchAction action = touchPicker.getTouchAction();
                    actions.set(index, action);
                    binding.touchInclude.touchPath.setPaths(action.getPaths(context));
                }, touchAction).show();
            });

            binding.touchInclude.playButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                TouchAction touchAction = (TouchAction) actions.get(index);
                MainAccessibilityService service = MainApplication.getService();
                if (touchAction.isValid()) {
                    if (service != null) {
                        Task task = new Task(new Behavior(touchAction));
                        service.runTask(task, null);
                    } else {
                        Toast.makeText(context, R.string.capture_service_on_tips_3, Toast.LENGTH_SHORT).show();
                    }
                }
            });

            binding.imageInclude.similarText.addTextChangedListener(new TextChangedListener() {
                @Override
                public void afterTextChanged(Editable s) {
                    int index = getBindingAdapterPosition();
                    ImageAction imageAction = (ImageAction) actions.get(index);
                    if (s != null && s.length() > 0) {
                        imageAction.setValue(Integer.parseInt(String.valueOf(s)));
                    } else {
                        imageAction.setValue(95);
                    }
                }
            });

            binding.imageInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                ImageAction imageAction = (ImageAction) actions.get(index);
                new ImagePickerFloatView(context, picker -> {
                    ImagePickerFloatView imagePicker = (ImagePickerFloatView) picker;
                    Bitmap bitmap = imagePicker.getBitmap();
                    imageAction.setBitmap(bitmap, DisplayUtils.getScreen(context));
                    binding.imageInclude.image.setImageBitmap(bitmap);
                }, imageAction).show();
            });

            binding.colorInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                ColorAction colorAction = (ColorAction) actions.get(index);
                new ColorPickerFloatView(context, picker -> {
                    ColorPickerFloatView colorPicker = (ColorPickerFloatView) picker;
                    ColorAction color = colorPicker.getColor();
                    actions.set(index, color);
                    binding.colorInclude.colorCard.setCardBackgroundColor(DisplayUtils.getColorFromHsv(color.getColor()));
                    binding.colorInclude.similarText.setText(color.getDescription(null, null, null));
                }, colorAction).show();
            });

            binding.inputInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                InputAction inputAction = (InputAction) actions.get(index);
                new InputPickerFloatView(context, picker -> {
                    InputPickerFloatView inputPicker = (InputPickerFloatView) picker;
                    String word = inputPicker.getInput().getId();
                    if (word != null){
                        inputAction.setId(word);
                        binding.inputInclude.textBaseInclude.titleEdit.setText(word);
                    }
                }, inputAction).show();
            });
            binding.inputInclude.textBaseInclude.titleEdit.setEnabled(false);
            binding.inputInclude.textBaseInclude.textInputLayout.setHint(context.getString(R.string.input_box));
            binding.inputInclude.textInputInclude.textInputLayout.setHint(context.getString(R.string.input_string));

            binding.inputInclude.textInputInclude.titleEdit.addTextChangedListener(new TextChangedListener() {
                @Override
                public void afterTextChanged(Editable s) {
                    int index = getBindingAdapterPosition();
                    Action action = actions.get(index);
                    if (action.getType() == ActionType.INPUT) ((InputAction) action).setText(String.valueOf(s));
                }
            });

            adapter = new ArrayAdapter<>(context, R.layout.float_action_spinner_item);
            binding.spinnerInclude.spinner.setAdapter(adapter);
            binding.spinnerInclude.spinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
                @Override
                public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                    int index = getBindingAdapterPosition();
                    Action action = actions.get(index);
                    TaskAction taskInfo = adapter.getItem(position);

                    if (action.getType() == ActionType.SYSTEM) {
                        SystemAction systemAction = (SystemAction) action;
                        SystemAction.SystemActionType systemActionType = SystemAction.SystemActionType.valueOf(taskInfo.getId());
                        if (systemActionType != systemAction.getSystemActionType()) {
                            systemAction.setSystemActionType(systemActionType);
                        }

                        if (systemActionType == SystemAction.SystemActionType.GOTO) {
                            binding.spinnerInclude.pickerButton.setVisibility(View.VISIBLE);
                            binding.spinnerInclude.image.setVisibility(View.VISIBLE);

                            MainViewModel viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
                            if (systemAction.getExtras() != null) {
                                AppInfo info = viewModel.getAppInfoByPkgName(systemAction.getExtras());
                                if (info != null) {
                                    PackageManager manager = context.getPackageManager();
                                    binding.spinnerInclude.image.setImageDrawable(info.info.applicationInfo.loadIcon(manager));
                                }
                            }

                        } else {
                            binding.spinnerInclude.pickerButton.setVisibility(View.GONE);
                            binding.spinnerInclude.image.setVisibility(View.GONE);
                        }
                    } else if (action.getType() == ActionType.TASK) {
                        TaskAction taskAction = (TaskAction) action;
                        taskAction.setId(taskInfo.getId());
                        taskAction.setTitle(taskInfo.getTitle());
                    }
                }

                @Override
                public void onNothingSelected(AdapterView<?> parent) {

                }
            });

            binding.spinnerInclude.pickerButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                SystemAction systemAction = (SystemAction) actions.get(index);
                new AppPickerFloatView(context, picker -> {
                    AppPickerFloatView appPicker = (AppPickerFloatView) picker;
                    AppInfo info = appPicker.getSelectApp();
                    if (info != null) {
                        systemAction.setExtras(info.packageName);

                        PackageManager manager = context.getPackageManager();
                        if (info.packageName.equals(context.getString(R.string.common_package_name))) {
                            binding.spinnerInclude.image.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
                        } else {
                            binding.spinnerInclude.image.setImageDrawable(info.info.applicationInfo.loadIcon(manager));
                        }
                    }
                }).show();
            });

            binding.spinnerInclude.image.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                SystemAction systemAction = (SystemAction) actions.get(index);
                String extras = systemAction.getExtras();
                if (extras == null || extras.isEmpty()) return;
                AppUtils.gotoApp(context, extras);
            });

            binding.upButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.max(0, index - 1);
                actions.add(newIndex, actions.remove(index));
                notifyItemRangeChanged(newIndex, 2);
            });

            binding.downButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.min(actions.size() - 1, index + 1);
                actions.add(newIndex, actions.remove(index));
                notifyItemRangeChanged(index, 2);
            });
        }

        public void refreshItem(Action action) {
            binding.titleText.setText(action.getType().getTypeName(context));
            binding.timeInclude.setValue(action.getTimeArea().getMin(), action.getTimeArea().getMax());

            switch (action.getType()) {
                case DELAY:
                    binding.delayInclude.setVisibility(View.VISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.inputInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.GONE);
                    binding.delayInclude.setValue(action.getTimeArea().getMin(), action.getTimeArea().getMax());
                    break;
                case COLOR:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.inputInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.VISIBLE);
                    binding.colorInclude.colorCard.setCardBackgroundColor(DisplayUtils.getColorFromHsv(((ColorAction) action).getColor()));
                    binding.colorInclude.similarText.setText(action.getDescription(null, null, null));
                    break;
                case TEXT:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.inputInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.VISIBLE);
                    binding.textInclude.textBaseInclude.titleEdit.setText(((TextAction) action).getText());
                    break;
                case TOUCH:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.inputInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.VISIBLE);
                    binding.touchInclude.touchPath.setPaths(((TouchAction) action).getPaths(context));
                    break;
                case IMAGE:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.inputInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.VISIBLE);
                    ImageAction imageAction = (ImageAction) action;
                    binding.imageInclude.similarText.setText(imageAction.getDescription(null, null, null));
                    if (imageAction.getBitmap() != null) {
                        binding.imageInclude.image.setImageBitmap(imageAction.getBitmap());
                    }
                    break;
                case INPUT:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.inputInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.timeInclude.setVisibility(View.GONE);
                    binding.inputInclude.textBaseInclude.titleEdit.setText(((InputAction) action).getId());
                    binding.inputInclude.textInputInclude.titleEdit.setText(((InputAction) action).getText());
                    break;
                case SYSTEM:
                case TASK:
                    binding.delayInclude.setVisibility(View.INVISIBLE);
                    binding.textInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.touchInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.imageInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.spinnerInclude.getRoot().setVisibility(View.VISIBLE);
                    binding.colorInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.inputInclude.getRoot().setVisibility(View.INVISIBLE);
                    binding.timeInclude.setVisibility(View.GONE);
                    if (action.getType() == ActionType.SYSTEM) {
                        adapter.clear();
                        for (SystemAction.SystemActionType systemActionType : SystemAction.SystemActionType.values()) {
                            adapter.add(new TaskAction(systemActionType.name(), systemActionType.getDescription(context, "")));
                        }
                        selectSpinner(((SystemAction) action).getSystemActionType().name());
                    } else {
                        adapter.clear();
                        List<Task> subTasks = baseTask.getSafeSubTasks(currTask.getId());
                        if (subTasks != null) {
                            for (Task taskItem : subTasks) {
                                adapter.add(new TaskAction(taskItem.getId(), taskItem.getTitle()));
                            }
                        }

                        TaskAction taskInfo = (TaskAction) action;
                        selectSpinner(taskInfo.getId());
                    }
                    break;
            }
        }

        private void initEditText(View view) {
            if (view instanceof ViewGroup) {
                ViewGroup viewGroup = (ViewGroup) view;
                for (int i = 0; i < viewGroup.getChildCount(); i++) {
                    initEditText(viewGroup.getChildAt(i));
                }
            } else {
                if (view instanceof EditText) {
                    EasyFloat.initInput((EditText) view, BehaviorFloatView.class.getCanonicalName());
                }
            }
        }

        private void selectSpinner(String id) {
            Spinner spinner = binding.spinnerInclude.spinner;
            for (int i = 0; i < adapter.getCount(); i++) {
                TaskAction item = adapter.getItem(i);
                if (item.getId().equals(id)) {
                    spinner.setSelection(i);
                    return;
                }
            }
            if (adapter.getCount() > 0) {
                spinner.setSelection(0);
                AdapterView.OnItemSelectedListener listener = spinner.getOnItemSelectedListener();
                listener.onItemSelected(spinner, spinner.getSelectedView(), 0, adapter.getItemId(0));
            }
        }
    }
}
