package top.bogey.auto_touch.ui.apps;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentAppsItemBinding;
import top.bogey.auto_touch.util.AppUtil;

public class AppsRecyclerViewAdapter extends RecyclerView.Adapter<AppsRecyclerViewAdapter.ViewHolder> {
    private final AppsFragment parent;
    private final List<AppInfo> appsInfo = new ArrayList<>();
    public final Map<String, Integer> flagMap = new HashMap<>();

    public AppsRecyclerViewAdapter(AppsFragment parent){
        this.parent = parent;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FragmentAppsItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, int position) {
        AppInfo info = appsInfo.get(position);
        holder.appName.setText(info.appName);
        holder.icon.setImageDrawable(AppUtil.getDrawable(parent.requireContext(), info.info));

        boolean existConfig = false;
        Integer count = flagMap.get(info.packageName);
        if (count != null){
            existConfig = count > 0;
            holder.numberText.setText(String.valueOf(count));
        }

        holder.numberText.setVisibility(existConfig ? View.VISIBLE : View.GONE);
        float alpha = existConfig ? 1f : 0.25f;
        holder.appName.setAlpha(alpha);
        holder.icon.setAlpha(alpha);
    }

    @Override
    public int getItemCount() {
        return appsInfo.size();
    }

    public void refreshList(List<AppInfo> appsInfo){
        int preSize = this.appsInfo.size();
        this.appsInfo.clear();
        notifyItemRangeRemoved(0, preSize);
        this.appsInfo.addAll(appsInfo);
        notifyItemRangeInserted(0, appsInfo.size());
    }

    public void notifyCountChanged(String pkgName, int count){
        flagMap.put(pkgName, count);

        for (int i = 0; i < appsInfo.size(); i++) {
            if (appsInfo.get(i).packageName.equals(pkgName)){
                notifyItemChanged(i);
                break;
            }
        }
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final TextView appName;
        public final TextView numberText;
        public final ImageView icon;
        public final ConstraintLayout layout;

        public ViewHolder(FragmentAppsItemBinding binding) {
            super(binding.getRoot());
            appName = binding.appName;
            numberText =binding.numberText;
            icon = binding.icon;
            layout = binding.getRoot();

            layout.setOnClickListener(view -> {
                int index = getBindingAdapterPosition();
                AppInfo info = appsInfo.get(index);
                NavController controller = Navigation.findNavController(parent.requireActivity(), R.id.con_view);
                controller.navigate(AppsFragmentDirections.actionAppsFragmentToTasksFragment(info.packageName));
            });
        }
    }
}