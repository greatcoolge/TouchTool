package top.bogey.touch_tool.ui.apps;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageManager;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewAppsItemBinding;
import top.bogey.touch_tool.room.bean.node.TaskNode;

public class AppsRecyclerViewAdapter extends RecyclerView.Adapter<AppsRecyclerViewAdapter.ViewHolder> {
    private final List<AppInfo> apps = new ArrayList<>();
    private final Map<String, Integer> flags = new HashMap<>();

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewAppsItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        AppInfo appInfo = apps.get(position);
        Integer count = flags.get(appInfo.packageName);
        holder.refreshView(appInfo, count == null ? 0 : count);
    }

    @Override
    public int getItemCount() {
        return apps.size();
    }

    public void refreshApps(List<AppInfo> apps) {
        this.apps.clear();
        if (apps != null) this.apps.addAll(apps);
        notifyDataSetChanged();
    }

    public void refreshItems(List<TaskNode.TaskGroup> group) {
        if (group == null || group.size() == 0) {
            for (Map.Entry<String, Integer> entry : flags.entrySet()) {
                refreshItem(entry.getKey(), 0);
            }
        } else {
            Set<String> keys = new HashSet<>();
            for (TaskNode.TaskGroup taskGroup : group) {
                refreshItem(taskGroup.getPkgName(), taskGroup.getCount());
                keys.add(taskGroup.getPkgName());
            }

            for (Map.Entry<String, Integer> entry : flags.entrySet()) {
                if (!keys.contains(entry.getKey())) {
                    refreshItem(entry.getKey(), 0);
                }
            }
        }
    }

    public void refreshItem(String pkgName, int count) {
        flags.put(pkgName, count);
        for (int i = 0; i < apps.size(); i++) {
            if (apps.get(i).packageName.equals(pkgName)) {
                notifyItemChanged(i);
                break;
            }
        }
    }

    protected static class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewAppsItemBinding binding;
        private final Context context;
        private AppInfo info;

        public ViewHolder(ViewAppsItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            binding.getRoot().setOnClickListener(v -> {
                MainActivity activity = MainApplication.getActivity();
                if (activity != null) {
                    NavController controller = Navigation.findNavController(activity, R.id.con_view);
                    controller.navigate(AppsViewDirections.actionAppsToTasks(info.packageName));
                }
            });
        }

        @SuppressLint("UseCompatLoadingForDrawables")
        public void refreshView(AppInfo appInfo, int count) {
            info = appInfo;
            binding.appName.setText(appInfo.appName);
            PackageManager manager = context.getPackageManager();
            if (appInfo.packageName.equals(context.getString(R.string.common_package_name))) {
                binding.icon.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
            } else {
                binding.icon.setImageDrawable(appInfo.info.applicationInfo.loadIcon(manager));
            }

            binding.numberText.setText(String.valueOf(count));
            binding.numberText.setVisibility(count > 0 ? View.VISIBLE : View.GONE);

            float alpha = count > 0 ? 1f : 0.25f;
            binding.getRoot().setAlpha(alpha);
        }
    }
}
