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
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentTasksBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.apps.AppInfo;
import top.bogey.auto_touch.ui.record.TaskRecordDialog;
import top.bogey.auto_touch.util.AppUtil;

public class TasksFragment extends Fragment {
    private String PKG_NAME = "";
    private FragmentTasksBinding binding;
    private MainViewModel viewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentTasksBinding.inflate(inflater, container, false);
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);

        TasksRecyclerViewAdapter adapter = new TasksRecyclerViewAdapter(this);
        binding.recyclerView.setAdapter(adapter);

        if (getArguments() != null){
            PKG_NAME = getArguments().getString("pkgName");
            viewModel.getTasksLiveByPackageName(PKG_NAME).observe(getViewLifecycleOwner(), tasks -> {
                if (tasks == null || tasks.isEmpty()){
                    binding.numberText.setVisibility(View.INVISIBLE);
                } else {
                    binding.numberText.setVisibility(View.VISIBLE);
                    binding.numberText.setText(String.valueOf(tasks.size()));
                }
                adapter.setTasks(tasks);
            });
            AppInfo info = viewModel.getAppInfoByPkgName(PKG_NAME);
            binding.appName.setText(info.appName);
            binding.packageName.setText(info.packageName);
            binding.icon.setImageDrawable(AppUtil.getDrawable(requireContext(), info.info));
        }

        return binding.getRoot();
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
    }

    @Override
    public void onCreateOptionsMenu(@NonNull Menu menu, @NonNull MenuInflater inflater) {
        inflater.inflate(R.menu.menu_tasks, menu);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @SuppressLint("NonConstantResourceId")
    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        switch (item.getItemId()) {
            case R.id.add:
                Task task = new Task();
                task.title = getString(R.string.task_default_title);
                task.pkgName = PKG_NAME;
                viewModel.saveTask(task);
                break;
            case R.id.record:
                new TaskRecordDialog(requireContext(), PKG_NAME).show();
                break;
        }
        return super.onOptionsItemSelected(item);
    }
}
