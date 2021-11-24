package top.bogey.auto_touch.ui.actions;


import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.SwitchCompat;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentActionsItemBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.action.ActionEditDialogFragment;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class ActionsRecyclerViewAdapter extends RecyclerView.Adapter<ActionsRecyclerViewAdapter.ViewHolder> {
    private final ActionsFragment parent;
    private final MainViewModel viewModel;
    private final List<Action> actions;
    private final Task task;

    public ActionsRecyclerViewAdapter(ActionsFragment parent, Task task){
        this.parent = parent;
        this.task = task;
        if (task.actions == null){
            task.actions = new ArrayList<>();
        }
        actions = task.actions;
        viewModel = new ViewModelProvider(parent.requireActivity()).get(MainViewModel.class);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FragmentActionsItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        if (actions.size() == 1) {
            holder.layout.setBackgroundResource(R.drawable.item_a);
            holder.up.setVisibility(View.INVISIBLE);
            holder.down.setVisibility(View.INVISIBLE);
        } else if (position == 0) {
            holder.layout.setBackgroundResource(R.drawable.item_f);
            holder.up.setVisibility(View.INVISIBLE);
            holder.down.setVisibility(View.VISIBLE);
        } else if (position == actions.size() - 1) {
            holder.layout.setBackgroundResource(R.drawable.item_l);
            holder.up.setVisibility(View.VISIBLE);
            holder.down.setVisibility(View.INVISIBLE);
        } else {
            holder.layout.setBackgroundResource(R.drawable.item_m);
            holder.up.setVisibility(View.VISIBLE);
            holder.down.setVisibility(View.VISIBLE);
        }
        Action action = actions.get(position);
        holder.title.setText(action.getTitle(parent.requireContext()));
        holder.enabledSwitch.setChecked(action.enable);
    }

    @Override
    public int getItemCount() {
        return actions.size();
    }

    public void setActions(List<Action> newActions){
        if (newActions == null){
            int size = actions.size();
            actions.clear();
            notifyItemRangeRemoved(0, size);
            return;
        }
        // 查找删除的 或 变更了的
        for (int i = actions.size() - 1; i >= 0; i--) {
            Action action = actions.get(i);
            boolean flag = true;
            for (Action newAction : newActions) {
                if (action.equals(newAction)) {
                    flag = false;
                    break;
                }
            }
            if (flag){
                actions.remove(i);
                notifyItemRemoved(i);
            }
        }

        // 查找新增的
        for (Action newAction : newActions) {
            boolean flag = true;
            for (Action action : actions) {
                if (action.equals(newAction)){
                    flag = false;
                    break;
                }
            }
            if (flag){
                actions.add(newAction);
                notifyItemInserted(actions.size() - 1);
                notifyItemChanged(Math.max(actions.size() - 2, 0));
            }
        }
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final ConstraintLayout layout;
        public final SwitchCompat enabledSwitch;
        public final TextView title;
        public final Button up;
        public final Button down;
        public final Button delete;


        public ViewHolder(FragmentActionsItemBinding binding) {
            super(binding.getRoot());
            layout = binding.getRoot();
            enabledSwitch = binding.enabledSwitch;
            title = binding.titleText;
            up = binding.upButton;
            down = binding.downButton;
            delete = binding.deleteButton;

            layout.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Action action = actions.get(index);
                new ActionEditDialogFragment(task, action, () -> {
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                }).show(parent.requireActivity().getSupportFragmentManager(), null);
            });

            enabledSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
                int index = getAdapterPosition();
                Action action = actions.get(index);
                action.enable = isChecked;
                viewModel.saveTask(task);
            });

            up.setOnClickListener(v -> {
                int index = getAdapterPosition();
                int newIndex = Math.max(0, index - 1);
                actions.add(newIndex, actions.remove(index));
                notifyItemRangeChanged(newIndex, 2);
                viewModel.saveTask(task);
            });

            down.setOnClickListener(v -> {
                int index = getAdapterPosition();
                int newIndex = Math.min(actions.size() - 1, index + 1);
                actions.add(newIndex, actions.remove(index));
                notifyItemRangeChanged(index, 2);
                viewModel.saveTask(task);
            });

            delete.setOnClickListener(v -> AppUtil.showSimpleDialog(parent.requireActivity(), R.string.delete_action_tips, new SelectCallback() {
                @Override
                public void onEnter() {
                    int index = getAdapterPosition();
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
                }

                @Override
                public void onCancel() { }
            }));
        }
    }
}