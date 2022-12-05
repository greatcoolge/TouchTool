package top.bogey.touch_tool.ui.setting;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import top.bogey.touch_tool.databinding.FloatLogItemBinding;

public class LogRecyclerViewAdapter extends RecyclerView.Adapter<LogRecyclerViewAdapter.ViewHolder> {
    private List<LogInfo> logs = new ArrayList<>();
    private final List<LogInfo> showLogs = new ArrayList<>();

    private RecyclerView recyclerView = null;
    private String searchText = "";

    private int level;

    public LogRecyclerViewAdapter(int level) {
        logs = LogUtils.getLogs(log -> recyclerView.post(() -> {
            logs.add(log);
            addLog(log);
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
        int size = showLogs.size();
        showLogs.clear();
        notifyItemRangeRemoved(0, size);
        for (LogInfo log : logs) {
            addLog(log);
        }
    }

    public void setSearchText(String searchText) {
        this.searchText = searchText;
        int size = showLogs.size();
        showLogs.clear();
        notifyItemRangeRemoved(0, size);
        for (LogInfo log : logs) {
            addLog(log);
        }
    }

    private void addLog(LogInfo log) {
        if (((1 << log.getLevel().ordinal()) & this.level) > 0) {
            Pattern pattern = Pattern.compile(searchText);
            Matcher matcher = pattern.matcher(log.getLog());
            if (searchText.isEmpty() || matcher.find()) {
                showLogs.add(log);
                notifyItemInserted(showLogs.size());
                if (recyclerView != null) recyclerView.scrollToPosition(showLogs.size() - 1);
            }
        }
    }

    public String getShowLogs() {
        StringBuilder builder = new StringBuilder();
        for (LogInfo log : showLogs) {
            builder.append(log.getLog());
            builder.append("\n");
        }
        return builder.toString();
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