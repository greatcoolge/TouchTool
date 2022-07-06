package top.bogey.auto_touch.ui.tasks;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.imageview.ShapeableImageView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.MainViewModel;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.ViewTasksActionBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.actions.ActionFloatView;
import top.bogey.auto_touch.utils.AppUtils;
import top.bogey.auto_touch.utils.DisplayUtils;
import top.bogey.auto_touch.utils.SelectCallback;

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
        private final MaterialButton enabledToggle;
        private final TextView title;
        private final ShapeableImageView modeImage;
        private boolean isDeleteMode = false;

        @SuppressLint("PrivateResource")
        public ViewHolder(ViewTasksActionBinding binding) {
            super(binding.getRoot());
            enabledToggle = binding.enabledToggle;
            title = binding.titleText;
            modeImage = binding.modeImage;

            itemView.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                new ActionFloatView(itemView.getContext(), task, action, result -> {
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                }).show();
            });

            binding.enabledToggle.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                action.setEnable(!action.isEnable());
                setChecked(enabledToggle, action.isEnable());
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
            title.setText(action.getDefaultTitle(itemView.getContext()));
            setChecked(enabledToggle, action.isEnable());
            enabledToggle.setText(String.valueOf(position + 1));
            switch (action.getActionMode()) {
                case CONDITION:
                    modeImage.setImageResource(R.drawable.icon_condition);
                    break;
                case LOOP:
                    modeImage.setImageResource(R.drawable.icon_loop);
                    break;
                case PARALLEL:
                    modeImage.setImageResource(R.drawable.icon_parallel);
                    break;
            }
        }
    }
}