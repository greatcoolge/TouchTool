package top.bogey.touch_tool.ui.setting;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.databinding.ViewRunningItemBinding;

public class RunningInfoRecyclerViewAdapter extends RecyclerView.Adapter<RunningInfoRecyclerViewAdapter.ViewHolder> {
    private final List<RunningInfo> runningInfoList = new ArrayList<>();

    public RunningInfoRecyclerViewAdapter(RunningInfoView parent) {
        Map<String, RunningInfo> runningInfoMap = RunningUtils.getRunningInfoMap();
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewRunningItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(runningInfoList.get(position));
    }

    @Override
    public int getItemCount() {
        return runningInfoList.size();
    }

    protected static class ViewHolder extends RecyclerView.ViewHolder {
        public final ViewRunningItemBinding binding;

        public ViewHolder(@NonNull ViewRunningItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }

        public void refreshItem(RunningInfo tip){
            binding.titleText.setText(tip.getTaskId());
        }
    }
}