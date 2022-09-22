package top.bogey.touch_tool.ui.setting;

import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import top.bogey.touch_tool.databinding.ViewDebugItemBinding;
import top.bogey.touch_tool.utils.LogUtils;

public class DebugInfoRecyclerViewAdapter extends RecyclerView.Adapter<DebugInfoRecyclerViewAdapter.ViewHolder> {
    private final List<DebugInfo> tips;

    public DebugInfoRecyclerViewAdapter(DebugInfoView parent) {
        tips = LogUtils.getLogs(parent.getContext(), null);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewDebugItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(tips.get(position));
    }

    @Override
    public int getItemCount() {
        return tips.size();
    }

    protected static class ViewHolder extends RecyclerView.ViewHolder {
        public final TextView title;

        public ViewHolder(@NonNull ViewDebugItemBinding binding) {
            super(binding.getRoot());
            title = binding.titleText;
        }

        public void refreshItem(DebugInfo tip){
            title.setText(tip.getDebugInfo());
            title.setTextColor(tip.getLevelColor(title.getContext()));
        }
    }
}