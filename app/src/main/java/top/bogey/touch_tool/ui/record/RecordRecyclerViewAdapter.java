package top.bogey.touch_tool.ui.record;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.action.Action;
import top.bogey.touch_tool.databinding.FloatRecordItemBinding;

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
        Behavior behavior = behaviors.get(position);
        Action target = behavior.getActions().get(0);
        switch (target.getType()) {
            case DELAY:
                holder.binding.deleteButton.setIconResource(R.drawable.icon_action_delay);
                break;
            case TEXT:
                holder.binding.deleteButton.setIconResource(R.drawable.icon_action_text);
                break;
            case IMAGE:
                holder.binding.deleteButton.setIconResource(R.drawable.icon_action_image);
                break;
            case TOUCH:
                holder.binding.deleteButton.setIconResource(R.drawable.icon_action_touch);
                break;
            case COLOR:
                holder.binding.deleteButton.setIconResource(R.drawable.icon_action_color);
                break;
            case SYSTEM:
                holder.binding.deleteButton.setIconResource(R.drawable.icon_action_system);
                break;
            case TASK:
                holder.binding.deleteButton.setIconResource(R.drawable.icon_task);
                break;
        }
        holder.binding.numberText.setText(String.valueOf(position + 1));
    }

    @Override
    public int getItemCount() {
        return behaviors.size();
    }

    public void addAction(@NonNull Behavior behavior) {
        behaviors.add(behavior);
        notifyItemInserted(behaviors.size() - 1);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final FloatRecordItemBinding binding;

        public ViewHolder(@NonNull FloatRecordItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;

            binding.deleteButton.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Behavior behavior = behaviors.get(index);
            });

            binding.deleteButton.setOnLongClickListener(v -> {
                int index = getBindingAdapterPosition();
                behaviors.remove(index);
                notifyDataSetChanged();
                return true;
            });
        }
    }
}