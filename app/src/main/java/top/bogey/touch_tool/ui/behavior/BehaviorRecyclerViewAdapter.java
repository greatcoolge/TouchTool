package top.bogey.touch_tool.ui.behavior;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.Editable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.ItemTouchHelper;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.snackbar.Snackbar;
import com.google.android.material.textfield.TextInputEditText;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTaskInfoBehaviorBinding;
import top.bogey.touch_tool.ui.task_info.TaskInfoView;
import top.bogey.touch_tool.utils.DisplayUtils;

public class BehaviorRecyclerViewAdapter extends RecyclerView.Adapter<BehaviorRecyclerViewAdapter.ViewHolder> {
    private final TaskInfoView parent;
    private final Task baseTask;
    private Task currTask;
    private List<Behavior> behaviors = new ArrayList<>();

    public BehaviorRecyclerViewAdapter(TaskInfoView parent, Task baseTask) {
        this.parent = parent;
        this.baseTask = baseTask;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewTaskInfoBehaviorBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(behaviors.get(position), position);
    }

    @Override
    public int getItemCount() {
        return behaviors.size();
    }

    public void setTask(Task task) {
        currTask = task;
        int count = getItemCount();
        behaviors = task.getBehaviors() == null ? new ArrayList<>() : task.getBehaviors();
        task.setBehaviors(behaviors);
        int size = behaviors.size();
        if (size == 0 && count > 0) notifyItemRangeRemoved(0, count);
        else {
            notifyItemRangeChanged(0, Math.min(size, count));
            if (size > count) notifyItemRangeInserted(count, size - count);
            else if (size < count) notifyItemRangeRemoved(size, count - size);
        }
    }

    public void notifyNew() {
        notifyItemInserted(behaviors.size() - 1);
    }

    private void saveTask() {
        TaskRepository.getInstance().saveTask(baseTask);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewTaskInfoBehaviorBinding binding;
        private final Context context;

        @SuppressLint("PrivateResource")
        public ViewHolder(ViewTaskInfoBehaviorBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            binding.getRoot().setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Behavior behavior = behaviors.get(index);
                new BehaviorFloatView(context, baseTask, currTask, behavior, result -> {
                    notifyItemChanged(index);
                    saveTask();
                }).show();
            });

            binding.enabledToggle.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Behavior behavior = behaviors.get(index);
                behavior.setEnable(!behavior.isEnable());
                setChecked(binding.enabledToggle, behavior.isEnable());
                saveTask();
            });

            binding.copyButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                MainViewModel viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
                viewModel.setCopyBehavior(behaviors.get(index));
            });

            binding.editButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Behavior behavior = behaviors.get(index);

                View view = LayoutInflater.from(context).inflate(R.layout.widget_text_input, null);
                TextInputEditText editText = view.findViewById(R.id.title_edit);
                editText.setText(behavior.getTitle(context, baseTask));

                new MaterialAlertDialogBuilder(context)
                        .setPositiveButton(R.string.enter, (dialog, which) -> {
                            Editable text = editText.getText();
                            if (text != null) behavior.setTitle(String.valueOf(text));
                            saveTask();
                            binding.titleText.setText(behavior.getTitle(context, currTask));
                            dialog.dismiss();
                        })
                        .setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss())
                        .setView(view)
                        .setTitle(R.string.behavior_custom_title_tips)
                        .show();
            });
        }

        @SuppressLint("PrivateResource")
        private void setChecked(MaterialButton button, boolean checked) {
            if (checked) {
                button.setTextColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorOnPrimary, 0));
                button.setBackgroundColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorPrimary, 0));
                button.setRippleColorResource(com.google.android.material.R.color.m3_button_ripple_color_selector);
            } else {
                button.setTextColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorOnSecondaryContainer, 0));
                button.setBackgroundColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorSecondaryContainer, 0));
                button.setRippleColorResource(com.google.android.material.R.color.m3_tonal_button_ripple_color_selector);
            }
        }

        public void refreshItem(Behavior behavior, int position) {
            binding.titleText.setText(behavior.getTitle(context, baseTask));
            setChecked(binding.enabledToggle, behavior.isEnable());
            binding.enabledToggle.setText(String.valueOf(position + 1));
            binding.modeImage.setImageResource(behavior.getBehaviorMode().getTypeResource());
        }
    }

    public static class BehaviorHelperCallback extends ItemTouchHelper.Callback {

        private static final int DRAG_FLAGS = ItemTouchHelper.UP | ItemTouchHelper.DOWN;
        private static final int SWIPE_FLAGS = ItemTouchHelper.START | ItemTouchHelper.END;

        private final BehaviorRecyclerViewAdapter adapter;

        public BehaviorHelperCallback(BehaviorRecyclerViewAdapter adapter) {
            this.adapter = adapter;
        }

        @Override
        public int getMovementFlags(@NonNull RecyclerView recyclerView, @NonNull RecyclerView.ViewHolder viewHolder) {
            return makeMovementFlags(DRAG_FLAGS, SWIPE_FLAGS);
        }

        @Override
        public boolean onMove(@NonNull RecyclerView recyclerView, @NonNull RecyclerView.ViewHolder viewHolder, @NonNull RecyclerView.ViewHolder target) {
            int startIndex = viewHolder.getBindingAdapterPosition();
            int targetIndex = target.getBindingAdapterPosition();

            Behavior behavior = adapter.behaviors.get(startIndex);
            adapter.behaviors.set(startIndex, adapter.behaviors.get(targetIndex));
            adapter.behaviors.set(targetIndex, behavior);
            adapter.notifyItemMoved(startIndex, targetIndex);
            adapter.saveTask();

            ((ViewHolder) viewHolder).binding.enabledToggle.setText(String.valueOf(targetIndex + 1));
            ((ViewHolder) target).binding.enabledToggle.setText(String.valueOf(startIndex + 1));

            return true;
        }

        @Override
        public void onSwiped(@NonNull RecyclerView.ViewHolder viewHolder, int direction) {
            int index = viewHolder.getBindingAdapterPosition();
            Behavior behavior = adapter.behaviors.remove(index);
            adapter.notifyItemRemoved(index);
            if (adapter.behaviors.size() > index) {
                adapter.notifyItemRangeChanged(index, adapter.behaviors.size() - index + 1);
            }
            adapter.saveTask();
            Snackbar.make(adapter.parent.requireView(), R.string.behavior_delete_tips, Snackbar.LENGTH_INDEFINITE)
                    .setAction(R.string.behavior_delete_reset, v -> {
                        adapter.behaviors.add(index, behavior);
                        adapter.notifyItemInserted(index);
                        if (adapter.behaviors.size() > index + 1) {
                            adapter.notifyItemRangeChanged(index + 1, adapter.behaviors.size() - index);
                        }
                        adapter.saveTask();
                    }).show();
        }
    }
}