package top.bogey.auto_touch.ui.apps;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.SearchView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import top.bogey.auto_touch.MainViewModel;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.ViewAppsBinding;

public class AppsView extends Fragment {
    private MainViewModel viewModel;
    private AppsRecyclerViewAdapter adapter;
    private String searchText = "";

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
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                if (menuItem.getItemId() == R.id.show_system) {
                    viewModel.showSystem.setValue(!Boolean.TRUE.equals(viewModel.showSystem.getValue()));
                    viewModel.refreshAppList();
                    adapter.refreshApps(viewModel.searchAppList(searchText));
                    return true;
                }
                return false;
            }
        }, getViewLifecycleOwner());

        ViewAppsBinding binding = ViewAppsBinding.inflate(inflater, container, false);
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
        adapter = new AppsRecyclerViewAdapter();
        binding.appsView.setAdapter(adapter);
        adapter.refreshApps(viewModel.searchAppList(""));
        searchText = "";

        viewModel.taskGroups.observe(getViewLifecycleOwner(), taskGroups -> adapter.refreshItems(taskGroups));
        return binding.getRoot();
    }
}
