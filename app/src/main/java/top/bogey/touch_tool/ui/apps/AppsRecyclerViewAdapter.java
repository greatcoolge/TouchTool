package top.bogey.touch_tool.ui.apps;

import android.content.Context;
import android.content.pm.PackageManager;
import android.graphics.drawable.Drawable;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewAppsItemBinding;

public class AppsRecyclerViewAdapter extends RecyclerView.Adapter<AppsRecyclerViewAdapter.ViewHolder> {
    private final MainViewModel viewModel;
    private final SelectAppCallback callback;

    private final List<AppInfo> apps = new ArrayList<>();
    private final Map<String, Drawable> drawables = new HashMap<>();
    private final Map<String, AppInfo> selectedApps = new HashMap<>();

    public AppsRecyclerViewAdapter(SelectAppCallback callback) {
        this.callback = callback;
        viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewAppsItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        AppInfo appInfo = apps.get(position);
        holder.refreshView(appInfo);
    }

    @Override
    public int getItemCount() {
        return apps.size();
    }

    public void refreshApps(List<AppInfo> newApps) {
        if (newApps == null || newApps.size() == 0) {
            int size = apps.size();
            apps.clear();
            notifyItemRangeRemoved(0, size);
            return;
        }

        for (int i = apps.size() - 1; i >= 0; i--) {
            AppInfo info = apps.get(i);
            boolean flag = true;
            for (AppInfo newApp : newApps) {
                if (info.packageName.equals(newApp.packageName)) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                apps.remove(i);
                notifyItemRemoved(i);
            }
        }

        for (int i = 0; i < newApps.size(); i++) {
            AppInfo newApp = newApps.get(i);
            boolean flag = true;
            for (AppInfo info : apps) {
                if (info.packageName.equals(newApp.packageName)) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                apps.add(i, newApp);
                notifyItemInserted(i);
            }
        }
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewAppsItemBinding binding;
        private final Context context;
        private AppInfo info;

        public ViewHolder(ViewAppsItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            binding.getRoot().setOnClickListener(v -> {
                if (selectedApps.remove(info.packageName) == null) selectedApps.put(info.packageName, info);
                callback.onSelectApps(new ArrayList<>(selectedApps.values()));
            });
        }

        public void refreshView(AppInfo appInfo) {
            info = appInfo;
            binding.appName.setText(appInfo.appName);
            PackageManager manager = context.getPackageManager();
            if (appInfo.packageName.equals(context.getString(R.string.common_package_name))) {
                binding.icon.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
            } else {
                binding.icon.setImageDrawable(appInfo.info.applicationInfo.loadIcon(manager));
            }
        }
    }
}
