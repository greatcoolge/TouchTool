package top.bogey.touch_tool.ui.setting;

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
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.data.TaskRepository;
import top.bogey.touch_tool.ui.apps.AppInfo;

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

    public void initRoot(){
        ArrayList<TreeNode> treeNodes = new ArrayList<>();
        Map<String, Map<String, List<RunningInfo>>> pkgMap = RunningUtils.getRunningInfo();
        for (Map.Entry<String, Map<String, List<RunningInfo>>> pkgEntry : pkgMap.entrySet()) {
            Map<String, List<RunningInfo>> taskMap = pkgEntry.getValue();
            TreeNode pkgTreeNode = new TreeNode(new TreeNodeInfo(pkgEntry.getKey(), pkgEntry.getValue().size()), R.layout.view_setting_running_app_item);
            for (Map.Entry<String, List<RunningInfo>> taskEntry : taskMap.entrySet()) {
                List<RunningInfo> taskList = taskEntry.getValue();
                TreeNode taskTreeNode = new TreeNode(new TreeNodeInfo(taskEntry.getKey(), taskEntry.getValue().size()), R.layout.view_setting_running_task_item);
                for (RunningInfo runningInfo : taskList) {
                    TreeNode infoTreeNode = new TreeNode(runningInfo, R.layout.view_setting_running_item);
                    taskTreeNode.addChild(infoTreeNode);
                }
                pkgTreeNode.addChild(taskTreeNode);
            }
            treeNodes.add(pkgTreeNode);
        }
        updateTreeNodes(treeNodes);
    }

    protected static class ViewHolder extends TreeViewHolder{
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

        public void refreshItem(TreeNode node, MainViewModel viewModel){
            int level = node.getLevel();
            PackageManager manager = context.getPackageManager();
            if (level == 0) {
                TreeNodeInfo nodeInfo = (TreeNodeInfo) node.getValue();
                AppInfo appInfo = viewModel.getAppInfoByPkgName(nodeInfo.getKey());
                if (appInfo != null){
                    appBinding.appName.setText(appInfo.appName);
                    if (appInfo.packageName.equals(context.getString(R.string.common_package_name))){
                        appBinding.icon.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
                    } else {
                        appBinding.icon.setImageDrawable(appInfo.info.applicationInfo.loadIcon(manager));
                    }
                }
                appBinding.numberText.setText(String.valueOf(nodeInfo.getValue()));
            } else if (level == 1) {
                TreeNodeInfo nodeInfo = (TreeNodeInfo) node.getValue();
                List<Task> taskList = TaskRepository.getInstance(context).getTasksById(nodeInfo.getKey());
                if (taskList != null && taskList.size() > 0){
                    Task task = taskList.get(0);
                    taskBinding.appName.setText(task.getTitle());
                    AppInfo appInfo = viewModel.getAppInfoByPkgName(task.getPkgName());
                    if (appInfo != null){
                        if (appInfo.packageName.equals(context.getString(R.string.common_package_name))){
                            taskBinding.icon.setImageDrawable(context.getApplicationInfo().loadIcon(manager));
                        } else {
                            taskBinding.icon.setImageDrawable(appInfo.info.applicationInfo.loadIcon(manager));
                        }
                    }
                }
                taskBinding.numberText.setText(String.valueOf(nodeInfo.getValue()));
            } else if (level == 2) {
                RunningInfo runningInfo = (RunningInfo) node.getValue();
                binding.time.setText(runningInfo.getDateString());
                binding.result.setText(context.getString(R.string.log_run_task_result, context.getString(runningInfo.isSuccess() ? R.string.log_run_task_result_success : R.string.log_run_task_result_fail)));
                binding.result.setTextColor((runningInfo.isSuccess() ? LogLevel.HIGH : LogLevel.LOW).getLevelColor(context));
            }
        }
    }
}
