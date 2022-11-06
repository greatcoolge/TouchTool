package top.bogey.touch_tool.ui.tasks;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.StaggeredGridLayoutManager;

import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTasksBinding;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.TaskChangedCallback;

public class TasksView extends Fragment implements TaskChangedCallback {

    private ViewTasksBinding binding;
    private TasksRecyclerViewAdapter adapter;

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        refreshSpawnCount();
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = ViewTasksBinding.inflate(inflater, container, false);

        binding.addButton.setOnClickListener(v -> {
            Task task = new Task(requireContext());
            TaskRepository.getInstance().saveTask(task);
        });

        adapter = new TasksRecyclerViewAdapter();
        binding.tasksBox.setAdapter(adapter);

        requireActivity().addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menuInflater.inflate(R.menu.menu_tasks, menu);
            }

            @SuppressLint("NonConstantResourceId")
            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                MainActivity activity = MainApplication.getActivity();
                switch (menuItem.getItemId()) {
                    case R.id.import_tasks:
                        if (activity != null) {
                            activity.launcherContent((code, intent) -> {
                                if (code == Activity.RESULT_OK) {
                                    Uri uri = intent.getData();
                                    if (uri != null) {
                                        activity.saveTasksByFile(uri);
                                    }
                                }
                            });
                        }
                        return true;
                    case R.id.export_tasks:
                        if (activity != null) {

                        }
                        return true;
                    case R.id.create_time:
                        adapter.setSortType(ShotType.CREATE_TIME_ASC);
                        break;
                    case R.id.modify_time:
                        adapter.setSortType(ShotType.MODIFY_TIME_ASC);
                        break;
                    default:
                        return false;
                }
                return true;
            }
        }, getViewLifecycleOwner());

        return binding.getRoot();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        TaskRepository.getInstance().removeCallback(this);
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        TaskRepository.getInstance().addCallback(this);
    }

    private void refreshSpawnCount() {
        StaggeredGridLayoutManager layoutManager = (StaggeredGridLayoutManager) binding.tasksBox.getLayoutManager();
        if (layoutManager != null) {
            if (!DisplayUtils.isPortrait(requireContext())) layoutManager.setSpanCount(4);
            else layoutManager.setSpanCount(2);
        }
    }

    @Override
    public void onChanged(Task task) {
        if (adapter != null) adapter.onChanged(task);
    }

    @Override
    public void onRemoved(Task task) {
        if (adapter != null) adapter.onRemoved(task);
    }
}
