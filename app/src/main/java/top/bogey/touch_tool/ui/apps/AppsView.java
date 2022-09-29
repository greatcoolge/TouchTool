package top.bogey.touch_tool.ui.apps;

import android.annotation.SuppressLint;
import android.content.res.Configuration;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.SearchView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.recyclerview.widget.GridLayoutManager;

import java.util.List;

import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewAppsBinding;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.data.TaskRepository;
import top.bogey.touch_tool.utils.DisplayUtils;

public class AppsView extends Fragment {
    private ViewAppsBinding binding;
    private MainViewModel viewModel;
    private AppsRecyclerViewAdapter adapter;
    private String searchText = "";

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        refreshSpawnCount();
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        requireActivity().addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menuInflater.inflate(R.menu.menu_apps, menu);
                MenuItem searchItem = menu.findItem(R.id.search_apps);
                SearchView searchView = (SearchView) searchItem.getActionView();
                searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
                    @Override
                    public boolean onQueryTextSubmit(String query) {
                        return false;
                    }

                    @Override
                    public boolean onQueryTextChange(String newText) {
                        searchText = newText;
                        adapter.refreshApps(viewModel.searchAppList(newText));
                        return true;
                    }
                });

                MenuItem showSystemItem = menu.findItem(R.id.show_system);
                showSystemItem.setChecked(Boolean.TRUE.equals(viewModel.showSystem.getValue()));
            }

            @SuppressLint("NonConstantResourceId")
            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                List<Task> tasks = TaskRepository.getInstance(getContext()).getAllTasks();
                switch (menuItem.getItemId()) {
                    case R.id.show_system:
                        viewModel.showSystem.setValue(!Boolean.TRUE.equals(viewModel.showSystem.getValue()));
                        menuItem.setChecked(Boolean.TRUE.equals(viewModel.showSystem.getValue()));
                        viewModel.refreshAppList();
                        adapter.refreshApps(viewModel.searchAppList(searchText));
                        return true;
                    case R.id.export_actions:
                        MainActivity activity = MainApplication.getActivity();
                        if (activity != null){
                            NavController controller = Navigation.findNavController(activity, R.id.con_view);
                            controller.navigate(AppsViewDirections.actionAppsToTasksExport());
                        }
                        return true;
                    case R.id.clean_actions:
                        List<String> pkgNames = viewModel.getAllPkgNames();
                        for (Task task : tasks) {
                            if (!pkgNames.contains(task.getPkgName())){
                                TaskRepository.getInstance(getContext()).deleteTask(task);
                            }
                        }
                        Toast.makeText(requireContext(), R.string.clean_tips, Toast.LENGTH_LONG).show();
                        return true;
                }
                return false;
            }
        }, getViewLifecycleOwner());

        binding = ViewAppsBinding.inflate(inflater, container, false);
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
        adapter = new AppsRecyclerViewAdapter();
        binding.appsView.setAdapter(adapter);
        refreshSpawnCount();
        adapter.refreshApps(viewModel.searchAppList(""));
        searchText = "";

        binding.refreshButton.setOnClickListener(v -> {
            viewModel.refreshAppList();
            adapter.refreshApps(viewModel.searchAppList(searchText));
        });

        viewModel.taskGroups.observe(getViewLifecycleOwner(), taskGroups -> adapter.refreshItems(taskGroups));
        return binding.getRoot();
    }

    private void refreshSpawnCount(){
        GridLayoutManager layoutManager = (GridLayoutManager) binding.appsView.getLayoutManager();
        if (layoutManager != null) {
            if (!DisplayUtils.isPortrait(requireContext()))layoutManager.setSpanCount(5);
            else layoutManager.setSpanCount(3);
        }
    }
}
