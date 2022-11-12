package top.bogey.touch_tool.ui.record;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.databinding.FloatRecordItemBinding;
import top.bogey.touch_tool.ui.behavior.BehaviorFloatView;

public class RecordRecyclerViewAdapter extends RecyclerView.Adapter<RecordRecyclerViewAdapter.ViewHolder> {
    private final Task task;
    public final List<Behavior> behaviors = new ArrayList<>();

    public RecordRecyclerViewAdapter(Task task) {
        this.task = task;
        if (task.getBehaviors() != null) behaviors.addAll(task.getBehaviors());
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatRecordItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(behaviors.get(position));
    }

    @Override
    public int getItemCount() {
        return behaviors.size();
    }

    public void addBehavior(Behavior behavior) {
        behaviors.add(behavior);
        notifyItemInserted(behaviors.size() - 1);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final FloatRecordItemBinding binding;
        private final Context context;

        public ViewHolder(@NonNull FloatRecordItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            binding.deleteButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Behavior behavior = behaviors.get(index);
                new BehaviorFloatView(context, task, behavior, null).show();
            });

            binding.deleteButton.setOnLongClickListener(v -> {
                int index = getBindingAdapterPosition();
                behaviors.remove(index);
                notifyItemRemoved(index);
                notifyItemRangeChanged(index, behaviors.size() - index);
                return true;
            });
        }

        public void refreshItem(Behavior behavior) {
            Action action = behavior.getActions().get(0);
            binding.deleteButton.setIconResource(action.getType().getTypeResource());
            binding.numberText.setText(String.valueOf(getBindingAdapterPosition() + 1));
        }
    }
}