package top.bogey.auto_touch.ui.record;

import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentRecordItemBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.action.FloatActionEdit;

public class RecordActionsRecyclerViewAdapter extends RecyclerView.Adapter<RecordActionsRecyclerViewAdapter.ViewHolder> {
    private final TaskRecordDialog parent;
    public final List<Action> actions = new ArrayList<>();

    public RecordActionsRecyclerViewAdapter(TaskRecordDialog parent, List<Action> actions){
        this.parent = parent;
        if (actions != null){
            this.actions.addAll(actions);
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatFragmentRecordItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        Action action = actions.get(position);
        Node target = action.getTargets().get(0);
        switch (target.getType()){
            case DELAY:
                holder.delete.setIconResource(R.drawable.delay);
            case TEXT:
                holder.delete.setIconResource(R.drawable.text);
                break;
            case IMAGE:
                holder.delete.setIconResource(R.drawable.image);
                break;
            case POS:
                holder.delete.setIconResource(R.drawable.pos);
                break;
            case KEY:
                holder.delete.setIconResource(R.drawable.keyboard);
                break;
            case TASK:
                holder.delete.setIconResource(R.drawable.task);
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

        public ViewHolder(@NonNull FloatFragmentRecordItemBinding binding) {
            super(binding.getRoot());
            delete = binding.deleteButton;
            indexText = binding.numberText;

            delete.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Action action = actions.get(index);
                Task task = parent.task;
                new FloatActionEdit(parent.getContext(), task, action, () -> notifyItemChanged(index)).show();
            });

            delete.setOnLongClickListener(v -> {
                int index = getAdapterPosition();
                actions.remove(index);
                notifyItemRemoved(index);
                return true;
            });
        }
    }
}