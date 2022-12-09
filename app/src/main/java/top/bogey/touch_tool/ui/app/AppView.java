package top.bogey.touch_tool.ui.app;

import android.os.Bundle;
import android.text.Editable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.material.bottomsheet.BottomSheetDialogFragment;

import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewAppBinding;
import top.bogey.touch_tool.utils.TextChangedListener;

public class AppView extends BottomSheetDialogFragment {
    private final Task task;
    private List<String> selectApps;
    private String searchText = "";

    public AppView(Task task) {
        this.task = task;
        selectApps = task.getPkgNames();
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        ViewAppBinding binding = ViewAppBinding.inflate(inflater, container, false);
        MainViewModel viewModel = MainViewModel.getInstance();

        List<AppInfo> appInfoList = new ArrayList<>();
        if (selectApps != null) {
            for (String pkgName : selectApps) {
                appInfoList.add(viewModel.getAppInfoByPkgName(requireContext(), pkgName));
            }
        }

        AppRecyclerViewAdapter adapter = new AppRecyclerViewAdapter(apps -> {
            selectApps = new ArrayList<>();
            for (AppInfo app : apps) {
                selectApps.add(app.packageName);
            }
            task.setPkgNames(selectApps);
            TaskRepository.getInstance().saveTask(task);
        }, appInfoList);
        binding.appBox.setAdapter(adapter);
        adapter.refreshApps(viewModel.searchAppList(requireContext(), searchText));

        binding.refreshButton.setOnClickListener(v -> {
            viewModel.showSystem.setValue(Boolean.FALSE.equals(viewModel.showSystem.getValue()));
            adapter.refreshApps(viewModel.searchAppList(requireContext(), searchText));
        });

        binding.titleEdit.addTextChangedListener(new TextChangedListener() {
            @Override
            public void afterTextChanged(Editable s) {
                searchText = s.toString();
                adapter.refreshApps(viewModel.searchAppList(requireContext(), searchText));
            }
        });

        return binding.getRoot();
    }
}
