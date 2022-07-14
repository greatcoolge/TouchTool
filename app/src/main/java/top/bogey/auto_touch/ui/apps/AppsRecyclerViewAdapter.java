package top.bogey.auto_touch.ui.apps;

import android.annotation.SuppressLint;
import android.content.pm.PackageManager;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.imageview.ShapeableImageView;
import com.google.android.material.textview.MaterialTextView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import top.bogey.auto_touch.MainActivity;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.ViewAppsItemBinding;
import top.bogey.auto_touch.room.bean.node.TaskNode;

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

    public void refreshApps(List<AppInfo> apps){
        this.apps.clear();
        if (apps != null) this.apps.addAll(apps);
        notifyDataSetChanged();
    }

    public void refreshItems(List<TaskNode.TaskGroup> group){
        if (group == null || group.size() == 0){
            for (Map.Entry<String, Integer> entry : flags.entrySet()) {
                refreshItem(entry.getKey(), 0);
            }
        } else {
            for (TaskNode.TaskGroup taskGroup : group) {
                refreshItem(taskGroup.getPkgName(), taskGroup.getCount());
            }
        }
    }

    public void refreshItem(String pkgName, int count){
        flags.put(pkgName, count);
        for (int i = 0; i < apps.size(); i++) {
            if (apps.get(i).packageName.equals(pkgName)){
                notifyItemChanged(i);
                break;
            }
        }
    }

    protected static class ViewHolder extends RecyclerView.ViewHolder{
        private final ViewAppsItemBinding binding;
        private AppInfo info;

        public ViewHolder(ViewAppsItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;

            itemView.setOnClickListener(v -> {
                MainActivity activity = MainApplication.getActivity();
                if (activity != null){
                    NavController controller = Navigation.findNavController(activity, R.id.con_view);
                    controller.navigate(AppsViewDirections.actionAppsToTasks(info.packageName));
                }
            });
        }

        @SuppressLint("UseCompatLoadingForDrawables")
        public void refreshView(AppInfo appInfo, int count){
            info = appInfo;
            binding.appName.setText(appInfo.appName);
            if (appInfo.packageName.equals(itemView.getContext().getString(R.string.common_package_name))){
                binding.icon.setImageDrawable(itemView.getContext().getDrawable(R.mipmap.ic_launcher));
            } else {
                PackageManager manager = itemView.getContext().getPackageManager();
                binding.icon.setImageDrawable(appInfo.info.applicationInfo.loadIcon(manager));
            }

            binding.numberText.setText(String.valueOf(count));
            binding.numberText.setVisibility(count > 0 ? View.VISIBLE : View.INVISIBLE);

            float alpha = count > 0 ? 1f : 0.25f;
            binding.appName.setAlpha(alpha * 2);
            binding.icon.setAlpha(alpha);
        }
    }
}
