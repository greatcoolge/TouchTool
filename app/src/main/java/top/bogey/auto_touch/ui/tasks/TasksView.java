package top.bogey.auto_touch.ui.tasks;

import android.annotation.SuppressLint;
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

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.MainViewModel;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.ViewTasksBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.apps.AppInfo;
import top.bogey.auto_touch.ui.record.QuickRecordFloatView;
import top.bogey.auto_touch.ui.record.RecordFloatView;

public class TasksView extends Fragment {
    private AppInfo appInfo = null;

    private MainViewModel viewModel;

    @SuppressLint("UseCompatLoadingForDrawables")
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
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
                    case R.id.add:
                        viewModel.saveTask(task);
                        break;
                    case R.id.record:
                        new RecordFloatView(requireContext(), task, result -> viewModel.saveTask(task)).show();
                        break;
                    case R.id.record_smart:
                        new QuickRecordFloatView(requireContext(), task, result -> viewModel.saveTask(task)).show();
                        break;
                    default:
                        return false;
                }
                return true;
            }
        }, getViewLifecycleOwner());

        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
        ViewTasksBinding binding = ViewTasksBinding.inflate(inflater, container, false);

        Bundle arguments = getArguments();
        if (arguments != null){
            String pkgName = arguments.getString("pkgName");
            appInfo = viewModel.getAppInfoByPkgName(pkgName);
        }

        ActionBar actionBar = MainApplication.getActivity().getSupportActionBar();
        if (actionBar != null) {
            actionBar.setTitle(appInfo.appName);
        }

        TasksRecyclerViewAdapter adapter = new TasksRecyclerViewAdapter();
        binding.tasksBox.setAdapter(adapter);

        viewModel.getTasksLiveByPackageName(appInfo.packageName).observe(getViewLifecycleOwner(), adapter::setTasks);

        viewModel.copyTask.observe(getViewLifecycleOwner(), task -> {
            if (task == null) binding.pasteButton.hide();
            else binding.pasteButton.show();
        });

        Task task = viewModel.getCopyTask();
        if (task == null) binding.pasteButton.hide();
        else binding.pasteButton.show();

        binding.pasteButton.setOnClickListener(v -> {
            Task copyTask = viewModel.getCopyTask();
            if (copyTask != null){
                copyTask.setPkgName(appInfo.packageName);
                viewModel.saveTask(copyTask);
            }
            viewModel.setCopyTask(null);
        });

        return binding.getRoot();
    }
}
