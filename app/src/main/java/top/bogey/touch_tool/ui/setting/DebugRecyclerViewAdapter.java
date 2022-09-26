package top.bogey.touch_tool.ui.setting;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.databinding.FloatDebugItemBinding;
import top.bogey.touch_tool.utils.LogUtils;

public class DebugRecyclerViewAdapter extends RecyclerView.Adapter<DebugRecyclerViewAdapter.ViewHolder> {
    private List<DebugInfo> tips = new ArrayList<>();
    private RecyclerView recyclerView = null;

    public DebugRecyclerViewAdapter(DebugFloatView parent) {
        tips = LogUtils.getLogs(parent.getContext(), log -> recyclerView.post(() -> {
            tips.add(log);
            notifyItemInserted(tips.size());
            if (recyclerView != null) recyclerView.scrollToPosition(tips.size() - 1);
        }));
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatDebugItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(tips.get(position));
    }

    @Override
    public void onAttachedToRecyclerView(@NonNull RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);
        this.recyclerView = recyclerView;
    }

    @Override
    public int getItemCount() {
        return tips.size();
    }

    protected static class ViewHolder extends RecyclerView.ViewHolder {
        public final FloatDebugItemBinding binding;

        public ViewHolder(@NonNull FloatDebugItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }

        public void refreshItem(DebugInfo tip){
            binding.titleText.setText(tip.getSimpleInfo());
            binding.titleText.setTextColor(tip.getLevelColor(binding.titleText.getContext()));
        }
    }
}