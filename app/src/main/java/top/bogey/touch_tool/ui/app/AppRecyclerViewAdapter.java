package top.bogey.touch_tool.ui.app;

import android.content.Context;
import android.content.pm.PackageManager;
import android.graphics.drawable.Drawable;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewAppItemBinding;

public class AppRecyclerViewAdapter extends RecyclerView.Adapter<AppRecyclerViewAdapter.ViewHolder> {
    private final SelectAppCallback callback;

    private final List<AppInfo> apps = new ArrayList<>();
    private final Map<String, AppInfo> selectedApps = new LinkedHashMap<>();
    private final Map<String, Drawable> icons = new HashMap<>();

    public AppRecyclerViewAdapter(SelectAppCallback callback, List<AppInfo> selectedApps) {
        this.callback = callback;
        if (selectedApps != null) {
            for (AppInfo appInfo : selectedApps) {
                if (appInfo != null) this.selectedApps.put(appInfo.packageName, appInfo);
            }
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(ViewAppItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
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
        private final ViewAppItemBinding binding;
        private final Context context;
        private final String commonPkgName;
        private AppInfo info;

        public ViewHolder(ViewAppItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();
            commonPkgName = context.getString(R.string.common_package_name);

            binding.getRoot().setOnClickListener(v -> {
                if (selectedApps.remove(info.packageName) == null)
                    selectedApps.put(info.packageName, info);
                List<AppInfo> infoList = new ArrayList<>(selectedApps.values());
                if (info.packageName.equals(commonPkgName)) {
                    for (AppInfo appInfo : infoList) {
                        for (int i = 0; i < apps.size(); i++) {
                            AppInfo info = apps.get(i);
                            if (appInfo.packageName.equals(info.packageName)) {
                                notifyItemChanged(i);
                                break;
                            }
                        }
                    }
                }
                notifyItemChanged(getBindingAdapterPosition());
                callback.onSelectApps(infoList);
            });
        }

        public void refreshView(AppInfo appInfo) {
            info = appInfo;
            Log.d("TAG", "refreshView: " + getBindingAdapterPosition());
            binding.appName.setText(appInfo.appName);
            binding.pkgName.setText(appInfo.packageName);
            PackageManager manager = context.getPackageManager();
            Drawable drawable = icons.get(appInfo.packageName);
            if (drawable == null) {
                if (appInfo.packageName.equals(context.getString(R.string.common_package_name))) {
                    drawable = context.getApplicationInfo().loadIcon(manager);
                } else {
                    drawable = appInfo.info.applicationInfo.loadIcon(manager);
                }
                icons.put(appInfo.packageName, drawable);
            }
            binding.icon.setImageDrawable(drawable);

            boolean containsKey = selectedApps.containsKey(commonPkgName);
            boolean isCommon = appInfo.packageName.equals(commonPkgName);

            binding.getRoot().setCheckedIconResource(isCommon || !containsKey ? R.drawable.icon_radio_selected : R.drawable.icon_radio_unselected);
            binding.getRoot().setChecked(selectedApps.containsKey(appInfo.packageName));
        }
    }
}
