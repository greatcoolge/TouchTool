package top.bogey.touch_tool.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageManager;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewAppsItemBinding;
import top.bogey.touch_tool.ui.apps.AppInfo;

public class AppPickerRecyclerViewAdapter extends RecyclerView.Adapter<AppPickerRecyclerViewAdapter.ViewHolder> {
    private final List<AppInfo> apps = new ArrayList<>();
    private final AppPickerFloatView parent;

    public AppPickerRecyclerViewAdapter(AppPickerFloatView parent) {
        this.parent = parent;
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

    public void refreshApps(List<AppInfo> apps) {
        this.apps.clear();
        if (apps != null) this.apps.addAll(apps);
        this.apps.remove(0);
        notifyDataSetChanged();
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        private final ViewAppsItemBinding binding;
        private final Context context;
        private AppInfo info;

        public ViewHolder(ViewAppsItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();

            binding.getRoot().setOnClickListener(v -> parent.setSelectApp(info));
        }

        @SuppressLint("UseCompatLoadingForDrawables")
        public void refreshView(AppInfo appInfo) {
            info = appInfo;
            binding.appName.setText(appInfo.appName);
            PackageManager manager = context.getPackageManager();
            if (appInfo.packageName.equals(context.getString(R.string.common_package_name))) {
                binding.icon.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
            } else {
                binding.icon.setImageDrawable(appInfo.info.applicationInfo.loadIcon(manager));
            }
            binding.numberText.setVisibility(View.GONE);
        }
    }
}
