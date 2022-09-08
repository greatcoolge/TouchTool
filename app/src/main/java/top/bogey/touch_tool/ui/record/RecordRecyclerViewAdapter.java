package top.bogey.touch_tool.ui.record;

import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.FloatRecordItemBinding;
import top.bogey.touch_tool.room.bean.Action;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.node.Node;
import top.bogey.touch_tool.ui.actions.ActionFloatView;

public class RecordRecyclerViewAdapter extends RecyclerView.Adapter<RecordRecyclerViewAdapter.ViewHolder> {
    private final Task task;
    public final List<Action> actions = new ArrayList<>();

    public RecordRecyclerViewAdapter(Task task){
        this.task = task;
        if (task.getActions() != null) actions.addAll(task.getActions());
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatRecordItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        Action action = actions.get(position);
        Node target = action.getTargets().get(0);
        switch (target.getType()){
            case DELAY:
                holder.delete.setIconResource(R.drawable.icon_delay);
                break;
            case TEXT:
                holder.delete.setIconResource(R.drawable.icon_text);
                break;
            case IMAGE:
                holder.delete.setIconResource(R.drawable.icon_image);
                break;
            case TOUCH:
                holder.delete.setIconResource(R.drawable.icon_touch);
                break;
            case COLOR:
                holder.delete.setIconResource(R.drawable.icon_color);
                break;
            case KEY:
                holder.delete.setIconResource(R.drawable.icon_key);
                break;
            case TASK:
                holder.delete.setIconResource(R.drawable.icon_task);
                break;
        }
        holder.indexText.setText(String.valueOf(position + 1));
    }

    @Override
    public int getItemCount() {
        return actions.size();
    }

    public void addAction(@NonNull Action action){
        actions.add(action);
        notifyItemInserted(actions.size() - 1);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final MaterialButton delete;
        public final TextView indexText;

        public ViewHolder(@NonNull FloatRecordItemBinding binding) {
            super(binding.getRoot());
            delete = binding.deleteButton;
            indexText = binding.numberText;

            delete.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                new ActionFloatView(itemView.getContext(), task, action, result -> notifyItemChanged(index)).show();
            });

            delete.setOnLongClickListener(v -> {
                int index = getBindingAdapterPosition();
                actions.remove(index);
                notifyDataSetChanged();
                return true;
            });
        }
    }
}