package top.bogey.touch_tool.ui.tasks;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.text.Editable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.textfield.TextInputEditText;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTaskInfoBehaviorBinding;
import top.bogey.touch_tool.ui.actions.ActionFloatView;
import top.bogey.touch_tool.utils.DisplayUtils;

public class BehaviorRecyclerViewAdapter extends RecyclerView.Adapter<BehaviorRecyclerViewAdapter.ViewHolder> {
    private final Task baseTask;
    private List<Behavior> behaviors = new ArrayList<>();

    public BehaviorRecyclerViewAdapter(Task baseTask) {
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

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewTaskInfoBehaviorBinding binding;
        private final Context context;
        private boolean isDeleteMode = false;

        @SuppressLint("PrivateResource")
        public ViewHolder(ViewTaskInfoBehaviorBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            binding.getRoot().setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Behavior behavior = behaviors.get(index);
                new ActionFloatView(context, baseTask, behavior, result -> {
                    notifyItemChanged(index);
                    TaskRepository.getInstance().saveTask(baseTask);
                }).show();
            });

            binding.getRoot().setOnLongClickListener(v -> {
                int index = getBindingAdapterPosition();
                Behavior behavior = behaviors.get(index);

                View view = LayoutInflater.from(context).inflate(R.layout.widget_text_input, null);
                TextInputEditText editText = view.findViewById(R.id.title_edit);
                editText.setText(behavior.getTitle(context));

                new MaterialAlertDialogBuilder(context)
                        .setPositiveButton(R.string.enter, (dialog, which) -> {
                            Editable text = editText.getText();
                            if (text != null) behavior.setTitle(String.valueOf(text));
                            TaskRepository.getInstance().saveTask(baseTask);
                            binding.titleText.setText(text);
                            dialog.dismiss();
                        })
                        .setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss())
                        .setView(view)
                        .setTitle(R.string.behavior_custom_title_tips)
                        .show();

                return true;
            });

            binding.enabledToggle.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Behavior behavior = behaviors.get(index);
                behavior.setEnable(!behavior.isEnable());
                setChecked(binding.enabledToggle, behavior.isEnable());
                TaskRepository.getInstance().saveTask(baseTask);
            });

            binding.upButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.max(0, index - 1);
                behaviors.add(newIndex, behaviors.remove(index));
                notifyItemRangeChanged(newIndex, 2);
                TaskRepository.getInstance().saveTask(baseTask);
            });

            binding.downButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.min(behaviors.size() - 1, index + 1);
                behaviors.add(newIndex, behaviors.remove(index));
                notifyItemRangeChanged(index, 2);
                TaskRepository.getInstance().saveTask(baseTask);
            });

            binding.deleteButton.setOnClickListener(v -> {
                if (isDeleteMode) {
                    int index = getBindingAdapterPosition();
                    behaviors.remove(index);
                    notifyItemRemoved(index);
                    if (!behaviors.isEmpty()) {
                        if (index == 0) {
                            notifyItemChanged(0);
                        } else if (index == behaviors.size()) {
                            notifyItemChanged(index - 1);
                        } else {
                            notifyItemChanged(index - 1);
                            notifyItemChanged(index);
                        }
                    }
                    TaskRepository.getInstance().saveTask(baseTask);
                } else {
                    isDeleteMode = true;
                    binding.deleteButton.setIconTint(ColorStateList.valueOf(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorError, 0)));
                    binding.deleteButton.setBackgroundTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorErrorContainer, 0)));
                    binding.deleteButton.postDelayed(() -> {
                        binding.deleteButton.setIconTintResource(com.google.android.material.R.color.m3_text_button_foreground_color_selector);
                        binding.deleteButton.setBackgroundTintList(ColorStateList.valueOf(context.getResources().getColor(android.R.color.transparent, null)));
                        isDeleteMode = false;
                    }, 3000);
                }
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
            binding.titleText.setText(behavior.getTitle(context));
            setChecked(binding.enabledToggle, behavior.isEnable());
            binding.enabledToggle.setText(String.valueOf(position + 1));
            binding.modeImage.setImageResource(behavior.getActionMode().getTypeResource());
        }
    }
}