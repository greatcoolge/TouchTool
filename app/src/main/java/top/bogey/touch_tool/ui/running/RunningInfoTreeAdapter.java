package top.bogey.touch_tool.ui.running;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageManager;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;

import com.amrdeveloper.treeview.TreeNode;
import com.amrdeveloper.treeview.TreeNodeManager;
import com.amrdeveloper.treeview.TreeViewAdapter;
import com.amrdeveloper.treeview.TreeViewHolder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewSettingRunningAppItemBinding;
import top.bogey.touch_tool.databinding.ViewSettingRunningItemBinding;
import top.bogey.touch_tool.databinding.ViewSettingRunningTaskItemBinding;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.ui.app.AppInfo;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.LogUtils;
import top.bogey.touch_tool.ui.setting.TreeNodeInfo;

public class RunningInfoTreeAdapter extends TreeViewAdapter {
    private final TreeNodeManager manager;
    private final MainViewModel viewModel;

    public RunningInfoTreeAdapter(TreeNodeManager manager) {
        super(null, manager);
        this.manager = manager;
        viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
        initRoot();
    }

    @Override
    public void onBindViewHolder(@NonNull TreeViewHolder holder, int position) {
        super.onBindViewHolder(holder, position);
        ((ViewHolder) holder).refreshItem(manager.get(position), viewModel);
    }

    @SuppressLint("NonConstantResourceId")
    @NonNull
    @Override
    public TreeViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int layoutId) {
        switch (layoutId) {
            case R.layout.view_setting_running_app_item:
                return new RunningInfoTreeAdapter.ViewHolder(ViewSettingRunningAppItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
            case R.layout.view_setting_running_item:
                return new RunningInfoTreeAdapter.ViewHolder(ViewSettingRunningItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
            default:
                return new RunningInfoTreeAdapter.ViewHolder(ViewSettingRunningTaskItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
        }
    }

    public void initRoot() {
        ArrayList<TreeNode> treeNodes = new ArrayList<>();
        Map<String, Map<String, List<RunningInfo>>> pkgMap = LogUtils.getRunningInfo();
        for (Map.Entry<String, Map<String, List<RunningInfo>>> pkgEntry : pkgMap.entrySet()) {
            String pkgName = pkgEntry.getKey();
            if (viewModel.getAppInfoByPkgName(pkgName) == null) {
                continue;
            }

            Map<String, List<RunningInfo>> taskMap = pkgEntry.getValue();
            TreeNode pkgTreeNode = new TreeNode(null, R.layout.view_setting_running_app_item);
            int totalTaskCount = 0;
            int totalSuccessCount = 0;
            for (Map.Entry<String, List<RunningInfo>> taskEntry : taskMap.entrySet()) {
                Task task = TaskRepository.getInstance().getTaskById(taskEntry.getKey());
                if (task == null) continue;

                List<RunningInfo> taskList = taskEntry.getValue();
                int taskCount = taskList.size();
                int successCount = 0;
                TreeNode taskTreeNode = new TreeNode(null, R.layout.view_setting_running_task_item);
                for (RunningInfo runningInfo : taskList) {
                    TreeNode infoTreeNode = new TreeNode(runningInfo, R.layout.view_setting_running_item);
                    taskTreeNode.addChild(infoTreeNode);
                    if (runningInfo.isSuccess()) successCount++;
                }
                totalTaskCount += taskCount;
                totalSuccessCount += successCount;
                taskTreeNode.setValue(new TreeNodeInfo(taskEntry.getKey(), taskCount, successCount));
                pkgTreeNode.addChild(taskTreeNode);
            }
            pkgTreeNode.setValue(new TreeNodeInfo(pkgName, totalTaskCount, totalSuccessCount));
            treeNodes.add(pkgTreeNode);
        }
        updateTreeNodes(treeNodes);
    }

    protected static class ViewHolder extends TreeViewHolder {
        private ViewSettingRunningAppItemBinding appBinding;
        private ViewSettingRunningItemBinding binding;
        private ViewSettingRunningTaskItemBinding taskBinding;
        private final Context context;

        public ViewHolder(@NonNull ViewSettingRunningAppItemBinding binding) {
            super(binding.getRoot());
            appBinding = binding;
            context = binding.getRoot().getContext();
            setNodePadding(0);
        }

        public ViewHolder(@NonNull ViewSettingRunningItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
            context = binding.getRoot().getContext();
            setNodePadding(0);
        }

        public ViewHolder(@NonNull ViewSettingRunningTaskItemBinding binding) {
            super(binding.getRoot());
            taskBinding = binding;
            context = binding.getRoot().getContext();
            setNodePadding(0);
        }

        @SuppressLint("DefaultLocale")
        public void refreshItem(TreeNode node, MainViewModel viewModel) {
            int level = node.getLevel();
            PackageManager manager = context.getPackageManager();
            if (level == 0) {
                TreeNodeInfo nodeInfo = (TreeNodeInfo) node.getValue();
                AppInfo appInfo = viewModel.getAppInfoByPkgName(nodeInfo.getKey());
                appBinding.appName.setText(appInfo.appName);
                if (appInfo.packageName.equals(context.getString(R.string.common_package_name))) {
                    appBinding.icon.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
                } else {
                    appBinding.icon.setImageDrawable(appInfo.info.applicationInfo.loadIcon(manager));
                }
                appBinding.numberText.setText(String.format("%d/%d", nodeInfo.getSuccess(), nodeInfo.getValue()));
            } else if (level == 1) {
                TreeNodeInfo nodeInfo = (TreeNodeInfo) node.getValue();
                Task task = TaskRepository.getInstance().getTaskById(nodeInfo.getKey());
                taskBinding.appName.setText(task.getTitle());
                taskBinding.numberText.setText(String.format("%d/%d", nodeInfo.getSuccess(), nodeInfo.getValue()));
            } else if (level == 2) {
                RunningInfo runningInfo = (RunningInfo) node.getValue();
                binding.time.setText(runningInfo.getDateString(context));
                binding.result.setText(context.getString(runningInfo.isSuccess() ? R.string.log_run_task_result_success : R.string.log_run_task_result_fail, ""));
                binding.result.setTextColor((runningInfo.isSuccess() ? LogLevel.HIGH : LogLevel.LOW).getLevelColor(context));
            }
        }
    }
}
