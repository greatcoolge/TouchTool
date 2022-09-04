package top.bogey.touch_tool.ui.debug;

import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.LinkedList;
import java.util.List;

import top.bogey.touch_tool.databinding.FloatDebugItemBinding;

public class DebugRecyclerViewAdapter extends RecyclerView.Adapter<DebugRecyclerViewAdapter.ViewHolder> {
    private final List<String> tips = new LinkedList<>();

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FloatDebugItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        holder.refreshItem(tips.get(position));
    }

    public void addTip(String tip){
        tips.add(tip);
        notifyItemInserted(tips.size());
        notifyItemChanged(tips.size() - 1);

        if (tips.size() > 500){
            removeTip(0);
        }
    }

    public void removeTip(int index){
        tips.remove(index);
        notifyItemRemoved(index);
        if (!tips.isEmpty()){
            if (index == 0){
                notifyItemChanged(0);
            } else if (index == tips.size()){
                notifyItemChanged(tips.size() - 1);
            } else {
                notifyItemChanged(index - 1);
                notifyItemChanged(index);
            }
        }
    }

    @Override
    public int getItemCount() {
        return tips.size();
    }

    protected static class ViewHolder extends RecyclerView.ViewHolder {
        public final TextView title;

        public ViewHolder(@NonNull FloatDebugItemBinding binding) {
            super(binding.getRoot());
            title = binding.titleText;
        }

        public void refreshItem(String tip){
            title.setText(tip);
        }
    }
}