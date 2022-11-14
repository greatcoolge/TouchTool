package top.bogey.touch_tool.ui.setting;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.databinding.FloatLogItemBinding;

public class LogRecyclerViewAdapter extends RecyclerView.Adapter<LogRecyclerViewAdapter.ViewHolder> {
    private List<LogInfo> logs = new ArrayList<>();
    private final List<LogInfo> showLogs = new ArrayList<>();

    private RecyclerView recyclerView = null;

    private int level;

    public LogRecyclerViewAdapter(int level) {
        logs = LogUtils.getLogs(log -> recyclerView.post(() -> {
            logs.add(log);
            if (((1 << log.getLevel().ordinal()) & this.level) > 0) {
                showLogs.add(log);
                notifyItemInserted(showLogs.size());
                if (recyclerView != null) recyclerView.scrollToPosition(showLogs.size() - 1);
            }
        }));
        setLevel(level);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatLogItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(showLogs.get(position));
    }

    @Override
    public void onAttachedToRecyclerView(@NonNull RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);
        this.recyclerView = recyclerView;
        recyclerView.scrollToPosition(getItemCount() - 1);
    }

    @Override
    public int getItemCount() {
        return showLogs.size();
    }

    public void setLevel(int level) {
        this.level = level;
        showLogs.clear();
        for (LogInfo log : logs) {
            if (((1 << log.getLevel().ordinal()) & level) > 0) {
                showLogs.add(log);
            }
        }
        notifyDataSetChanged();
        if (recyclerView != null) recyclerView.scrollToPosition(showLogs.size() - 1);
    }

    protected static class ViewHolder extends RecyclerView.ViewHolder {
        public final FloatLogItemBinding binding;
        private final Context context;

        public ViewHolder(@NonNull FloatLogItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();
        }

        public void refreshItem(LogInfo log) {
            binding.titleText.setText(String.format("%s\n%s", log.getDateString(context), log.getLog()));
            binding.titleText.setTextColor(log.getLevel().getLevelColor(binding.getRoot().getContext()));
        }
    }
}