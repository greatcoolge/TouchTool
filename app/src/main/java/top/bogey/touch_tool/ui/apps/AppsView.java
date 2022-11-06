package top.bogey.touch_tool.ui.apps;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetDialogFragment;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewAppsBinding;

public class AppsView extends BottomSheetDialogFragment {
    private ViewAppsBinding binding;
    private final Task task;
    private List<String> selectApps;

    public AppsView(Task task) {
        this.task = task;
        selectApps = task.getPkgNames();
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = ViewAppsBinding.inflate(inflater, container, false);
        MainViewModel viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);

        List<AppInfo> appInfoList = new ArrayList<>();
        if (selectApps != null){
            for (String pkgName : selectApps) {
                appInfoList.add(viewModel.getAppInfoByPkgName(pkgName));
            }
        }

        AppsRecyclerViewAdapter adapter = new AppsRecyclerViewAdapter(apps -> {
            selectApps = new ArrayList<>();
            for (AppInfo app : apps) {
                selectApps.add(app.packageName);
            }
            task.setPkgNames(selectApps);
            TaskRepository.getInstance().saveTask(task);
        }, appInfoList);
        binding.appBox.setAdapter(adapter);
        adapter.refreshApps(viewModel.searchAppList(""));

        return binding.getRoot();
    }
}
