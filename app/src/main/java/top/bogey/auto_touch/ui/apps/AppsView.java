package top.bogey.auto_touch.ui.apps;

import android.annotation.SuppressLint;
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
        ViewAppsBinding binding = ViewAppsBinding.inflate(inflater, container, false);
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
        adapter = new AppsRecyclerViewAdapter();
        binding.appsView.setAdapter(adapter);
        adapter.refreshApps(viewModel.searchAppList(""));
        searchText = "";

        viewModel.taskGroups.observe(getViewLifecycleOwner(), taskGroups -> adapter.refreshItems(taskGroups));
        return binding.getRoot();
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
    }

    @Override
    public void onCreateOptionsMenu(@NonNull Menu menu, @NonNull MenuInflater inflater) {
        inflater.inflate(R.menu.menu_apps, menu);
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

        super.onCreateOptionsMenu(menu, inflater);
    }

    @SuppressLint("NonConstantResourceId")
    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        switch (item.getItemId()){
            case R.id.show_system:
                viewModel.showSystem.setValue(!Boolean.TRUE.equals(viewModel.showSystem.getValue()));
                adapter.refreshApps(viewModel.searchAppList(searchText));
                break;
            case R.id.refresh_apps:
                viewModel.refreshAppList();
                adapter.refreshApps(viewModel.searchAppList(searchText));
                break;
        }
        return super.onOptionsItemSelected(item);
    }
}
