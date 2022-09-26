package top.bogey.touch_tool.ui.tasks;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.text.Editable;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewActionTextBaseBinding;
import top.bogey.touch_tool.databinding.ViewTasksActionBinding;
import top.bogey.touch_tool.room.bean.Action;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.ui.actions.ActionFloatView;
import top.bogey.touch_tool.utils.DisplayUtils;

public class ActionsRecyclerViewAdapter extends RecyclerView.Adapter<ActionsRecyclerViewAdapter.ViewHolder> {
    private final MainViewModel viewModel;
    private List<Action> actions = new ArrayList<>();
    private Task task;

    public ActionsRecyclerViewAdapter(){
        viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewTasksActionBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(actions.get(position), position);
    }

    @Override
    public int getItemCount() {
        return actions.size();
    }

    public void setTask(Task task){
        this.task = task;
        if (task.getActions() == null) task.setActions(new ArrayList<>());
        actions = task.getActions();
        notifyDataSetChanged();
    }

    public void notifyNew(){
        notifyItemInserted(actions.size() - 1);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewTasksActionBinding binding;
        private boolean isDeleteMode = false;

        @SuppressLint("PrivateResource")
        public ViewHolder(ViewTasksActionBinding binding) {
            super(binding.getRoot());
            this.binding = binding;

            itemView.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                new ActionFloatView(itemView.getContext(), task, action, result -> {
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                }).show();
            });

            itemView.setOnLongClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                ViewActionTextBaseBinding textBaseBinding = ViewActionTextBaseBinding.inflate(LayoutInflater.from(itemView.getContext()));
                textBaseBinding.textInputLayout.setHint(R.string.action_name);
                textBaseBinding.titleEdit.setText(action.getTitle());
                new MaterialAlertDialogBuilder(itemView.getContext())
                        .setPositiveButton(R.string.enter, (dialog, which) -> {
                            Editable text = textBaseBinding.titleEdit.getText();
                            if (text != null) action.setTitle(String.valueOf(text));
                            notifyItemChanged(index);
                            viewModel.saveTask(task);
                            dialog.dismiss();
                        })
                        .setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss())
                        .setView(textBaseBinding.getRoot())
                        .show();
                return true;
            });

            binding.enabledToggle.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                action.setEnable(!action.isEnable());
                setChecked(binding.enabledToggle, action.isEnable());
                viewModel.saveTask(task);
            });

            binding.upButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.max(0, index - 1);
                actions.add(newIndex, actions.remove(index));
                notifyItemRangeChanged(newIndex, 2);
                viewModel.saveTask(task);
            });

            binding.downButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.min(actions.size() - 1, index + 1);
                actions.add(newIndex, actions.remove(index));
                notifyItemRangeChanged(index, 2);
                viewModel.saveTask(task);
            });

            binding.deleteButton.setOnClickListener(v -> {
                if (isDeleteMode){
                    int index = getBindingAdapterPosition();
                    actions.remove(index);
                    notifyItemRemoved(index);
                    if (!actions.isEmpty()){
                        if (index == 0){
                            notifyItemChanged(0);
                        } else if (index == actions.size()){
                            notifyItemChanged(index - 1);
                        } else {
                            notifyItemChanged(index - 1);
                            notifyItemChanged(index);
                        }
                    }
                    viewModel.saveTask(task);
                } else {
                    isDeleteMode = true;
                    binding.deleteButton.setIconTint(ColorStateList.valueOf(DisplayUtils.getAttrColor(itemView.getContext(), com.google.android.material.R.attr.colorError, 0)));
                    binding.deleteButton.setBackgroundTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(itemView.getContext(), com.google.android.material.R.attr.colorErrorContainer, 0)));
                    binding.deleteButton.postDelayed(() -> {
                        binding.deleteButton.setIconTintResource(com.google.android.material.R.color.m3_text_button_foreground_color_selector);
                        binding.deleteButton.setBackgroundTintList(ColorStateList.valueOf(itemView.getContext().getResources().getColor(android.R.color.transparent, null)));
                        isDeleteMode = false;
                    }, 3000);
                }
            });
        }

        @SuppressLint("PrivateResource")
        private void setChecked(MaterialButton button, boolean checked){
            Context context = itemView.getContext();
            if (checked){
                button.setTextColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorOnPrimary, 0));
                button.setBackgroundColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorPrimary, 0));
                button.setRippleColorResource(com.google.android.material.R.color.m3_button_ripple_color_selector);
            } else {
                button.setTextColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorOnSecondaryContainer, 0));
                button.setBackgroundColor(DisplayUtils.getAttrColor(context, com.google.android.material.R.attr.colorSecondaryContainer, 0));
                button.setRippleColorResource(com.google.android.material.R.color.m3_tonal_button_ripple_color_selector);
            }
        }

        public void refreshItem(Action action, int position){
            binding.titleText.setText(action.getTitle(itemView.getContext()));
            setChecked(binding.enabledToggle, action.isEnable());
            binding.enabledToggle.setText(String.valueOf(position + 1));
            switch (action.getActionMode()) {
                case CONDITION:
                    binding.modeImage.setImageResource(R.drawable.icon_condition);
                    break;
                case LOOP:
                    binding.modeImage.setImageResource(R.drawable.icon_loop);
                    break;
                case PARALLEL:
                    binding.modeImage.setImageResource(R.drawable.icon_parallel);
                    break;
            }
        }
    }
}