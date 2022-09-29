package top.bogey.touch_tool.ui.setting;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.databinding.FloatLogItemBinding;

public class LogRecyclerViewAdapter extends RecyclerView.Adapter<LogRecyclerViewAdapter.ViewHolder> {
    private List<LogInfo> logs = new ArrayList<>();
    private RecyclerView recyclerView = null;

    public LogRecyclerViewAdapter(LogFloatView parent) {
        logs = RunningUtils.getLogs(parent.getContext(), log -> recyclerView.post(() -> {
            logs.add(log);
            notifyItemInserted(logs.size());
            if (recyclerView != null) recyclerView.scrollToPosition(logs.size() - 1);
        }));
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatLogItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(logs.get(position));
    }

    @Override
    public void onAttachedToRecyclerView(@NonNull RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);
        this.recyclerView = recyclerView;
        recyclerView.scrollToPosition(getItemCount() - 1);
    }

    @Override
    public int getItemCount() {
        return logs.size();
    }

    protected static class ViewHolder extends RecyclerView.ViewHolder {
        public final FloatLogItemBinding binding;

        public ViewHolder(@NonNull FloatLogItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }

        public void refreshItem(LogInfo log){
            binding.titleText.setText(String.format("%s\n%s", log.getDate(), log.getLog()));
            binding.titleText.setTextColor(log.getLevel().getLevelColor(binding.getRoot().getContext()));
        }
    }
}