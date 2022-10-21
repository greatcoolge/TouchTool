package top.bogey.touch_tool.ui.tasks;

import android.annotation.SuppressLint;
import android.content.res.Configuration;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.ActionBar;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.StaggeredGridLayoutManager;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewTasksBinding;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.room.data.TaskRepository;
import top.bogey.touch_tool.ui.apps.AppInfo;
import top.bogey.touch_tool.ui.record.QuickRecordFloatView;
import top.bogey.touch_tool.ui.record.RecordFloatView;
import top.bogey.touch_tool.utils.DisplayUtils;

public class TasksView extends Fragment {
    private AppInfo appInfo = null;

    private ViewTasksBinding binding;
    private MainViewModel viewModel;

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        refreshSpawnCount();
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {

        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
        binding = ViewTasksBinding.inflate(inflater, container, false);

        Bundle arguments = getArguments();
        if (arguments != null){
            String pkgName = arguments.getString("pkgName");
            appInfo = viewModel.getAppInfoByPkgName(pkgName);
        }

        requireActivity().addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menuInflater.inflate(R.menu.menu_tasks, menu);
            }

            @SuppressLint("NonConstantResourceId")
            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                Task task = new Task();
                task.setPkgName(appInfo.packageName);
                task.setTitle(getString(R.string.task_title_default));
                switch (menuItem.getItemId()) {
                    case R.id.add_special:
                        task.setAcrossApp(true);
                    case R.id.add:
                        TaskRepository.getInstance(getContext()).saveTask(task);
                        break;
                    case R.id.record:
                        new RecordFloatView(requireContext(), task, result -> TaskRepository.getInstance(getContext()).saveTask(task)).show();
                        break;
                    case R.id.record_smart:
                        new QuickRecordFloatView(requireContext(), task, result -> TaskRepository.getInstance(getContext()).saveTask(task)).show();
                        break;
                    default:
                        return false;
                }
                return true;
            }
        }, getViewLifecycleOwner());

        ActionBar actionBar = MainApplication.getActivity().getSupportActionBar();
        if (actionBar != null) {
            actionBar.setTitle(appInfo.appName);
            actionBar.setSubtitle(appInfo.packageName);
        }

        TasksRecyclerViewAdapter adapter = new TasksRecyclerViewAdapter();
        binding.tasksBox.setAdapter(adapter);
        refreshSpawnCount();

        TaskRepository.getInstance(getContext()).getTasksLiveByPackageName(appInfo.packageName).observe(getViewLifecycleOwner(), adapter::setTasks);

        viewModel.copyTask.observe(getViewLifecycleOwner(), task -> {
            if (task == null) {
                binding.pasteButton.hide();
                binding.pasteSpecialButton.hide();
            }
            else {
                binding.pasteButton.show();
                binding.pasteSpecialButton.show();
            }
        });

        Task task = viewModel.getCopyTask();
        if (task == null) {
            binding.pasteButton.hide();
            binding.pasteSpecialButton.hide();
        }
        else {
            binding.pasteButton.show();
            binding.pasteSpecialButton.show();
        }

        binding.pasteButton.setOnClickListener(v -> {
            Task copyTask = viewModel.getCopyTask();
            if (copyTask != null){
                copyTask.setPkgName(appInfo.packageName);
                if (copyTask.getStatus() == TaskStatus.TIME){
                    if (!appInfo.packageName.equals(getString(R.string.common_package_name))) copyTask.setStatus(TaskStatus.CLOSED);
                }
                TaskRepository.getInstance(getContext()).saveTask(copyTask, true);
            }
            viewModel.setCopyTask(null);
        });

        binding.pasteSpecialButton.setOnClickListener(v -> {
            Task copyTask = viewModel.getCopyTask();
            if (copyTask != null){
                copyTask.setPkgName(appInfo.packageName);
                if (copyTask.getStatus() == TaskStatus.TIME){
                    if (!appInfo.packageName.equals(getString(R.string.common_package_name))) copyTask.setStatus(TaskStatus.CLOSED);
                }
                copyTask.setAcrossApp(!copyTask.isAcrossApp());
                TaskRepository.getInstance(getContext()).saveTask(copyTask, true);
            }
            viewModel.setCopyTask(null);
        });

        binding.pasteButton.setOnLongClickListener(v -> {
            viewModel.setCopyTask(null);
            return true;
        });

        return binding.getRoot();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        ActionBar actionBar = MainApplication.getActivity().getSupportActionBar();
        if (actionBar != null) {
            actionBar.setSubtitle(null);
        }
    }

    private void refreshSpawnCount(){
        StaggeredGridLayoutManager layoutManager = (StaggeredGridLayoutManager) binding.tasksBox.getLayoutManager();
        if (layoutManager != null) {
            if (!DisplayUtils.isPortrait(requireContext()))layoutManager.setSpanCount(3);
            else layoutManager.setSpanCount(2);
        }
    }
}
