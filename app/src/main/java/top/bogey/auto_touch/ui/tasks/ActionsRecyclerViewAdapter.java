package top.bogey.auto_touch.ui.tasks;

import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;
import android.widget.ToggleButton;

import androidx.annotation.NonNull;
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
import top.bogey.auto_touch.ui.action.FloatActionEdit;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class ActionsRecyclerViewAdapter extends RecyclerView.Adapter<ActionsRecyclerViewAdapter.ViewHolder> {
    private final TasksFragment parent;
    private final MainViewModel viewModel;
    private List<Action> actions = new ArrayList<>();
    private Task task;

    public ActionsRecyclerViewAdapter(TasksFragment parent){
        this.parent = parent;
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
            holder.up.setVisibility(View.GONE);
            holder.down.setVisibility(View.GONE);
        } else if (position == 0) {
            holder.layout.setBackgroundResource(R.drawable.item_f);
            holder.up.setVisibility(View.GONE);
            holder.down.setVisibility(View.VISIBLE);
        } else if (position == actions.size() - 1) {
            holder.layout.setBackgroundResource(R.drawable.item_l);
            holder.up.setVisibility(View.VISIBLE);
            holder.down.setVisibility(View.GONE);
        } else {
            holder.layout.setBackgroundResource(R.drawable.item_m);
            holder.up.setVisibility(View.VISIBLE);
            holder.down.setVisibility(View.VISIBLE);
        }
        Action action = actions.get(position);
        holder.title.setText(action.getTitle(parent.requireContext()));
        holder.enabledToggle.setChecked(action.isEnable());
        holder.refreshSelectState(action.isEnable());
        holder.enabledToggle.setText(String.valueOf(position + 1));
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
        notifyItemChanged(actions.size() - 2);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final ConstraintLayout layout;
        public final ToggleButton enabledToggle;
        public final TextView title;
        public final Button up;
        public final Button down;
        public final Button delete;


        public ViewHolder(FragmentActionsItemBinding binding) {
            super(binding.getRoot());
            layout = binding.getRoot();
            enabledToggle = binding.enabledToggle;
            title = binding.titleText;
            up = binding.upButton;
            down = binding.downButton;
            delete = binding.deleteButton;

            layout.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Action action = actions.get(index);
                new FloatActionEdit(parent.requireContext(), task, action, () -> {
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                }).show();
            });

            enabledToggle.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Action action = actions.get(index);
                action.setEnable(!action.isEnable());
                refreshSelectState(action.isEnable());
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

        public void refreshSelectState(boolean isChecked){
            Context context = parent.requireContext();
            if (isChecked){
                enabledToggle.setBackgroundTintList(ColorStateList.valueOf(AppUtil.getGroupColor(context, task.getGroupId())));
            } else {
                int[] attrs = new int[] {R.attr.backgroundColor};
                TypedArray typedArray = context.getTheme().obtainStyledAttributes(attrs);
                int selectColor = typedArray.getResourceId(0, R.color.grey_300);
                typedArray.recycle();

                enabledToggle.setBackgroundTintList(ColorStateList.valueOf(context.getResources().getColor(selectColor, null)));
            }
        }
    }
}