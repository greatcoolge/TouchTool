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
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.GridLayoutManager;

import java.util.Map;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentAppsBinding;
import top.bogey.auto_touch.room.data.TaskGroup;
import top.bogey.auto_touch.ui.MainViewModel;

public class AppsFragment extends Fragment {
    private FragmentAppsBinding binding;
    private MainViewModel viewModel;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        binding = FragmentAppsBinding.inflate(inflater, container, false);
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
        binding.getRoot().setLayoutManager(new GridLayoutManager(requireContext(), 4));
        AppsRecyclerViewAdapter adapter = new AppsRecyclerViewAdapter(this);
        binding.getRoot().setAdapter(adapter);
        adapter.refreshList(viewModel.searchAppList(""));

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

        AppsRecyclerViewAdapter adapter = (AppsRecyclerViewAdapter) binding.getRoot().getAdapter();

        MenuItem searchViewItem = menu.findItem(R.id.search);
        SearchView searchView = (SearchView) searchViewItem.getActionView();
        searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
            @Override
            public boolean onQueryTextSubmit(String s) {
                return false;
            }

            @Override
            public boolean onQueryTextChange(String s) {
                if (adapter != null){
                    adapter.refreshList(viewModel.searchAppList(s));
                    return true;
                }
                return false;
            }
        });

        MenuItem refreshButtonItem = menu.findItem(R.id.refresh);
        refreshButtonItem.setOnMenuItemClickListener(menuItem -> {
            viewModel.refreshAppList();
            if (adapter != null){
                adapter.refreshList(viewModel.searchAppList(searchView.getQuery().toString()));
            }
            return true;
        });

        MenuItem showSystemButtonItem = menu.findItem(R.id.add);
        showSystemButtonItem.setOnMenuItemClickListener(menuItem -> {
            viewModel.showSystem.setValue(!viewModel.isShowSystem());
            if (adapter != null){
                adapter.refreshList(viewModel.searchAppList(searchView.getQuery().toString()));
            }
            return true;
        });

        viewModel.taskGroups.observe(getViewLifecycleOwner(), taskGroups -> {
            if (adapter != null){
                if (taskGroups == null || taskGroups.size() == 0){
                    for (Map.Entry<String, Integer> entry: adapter.flagMap.entrySet()) {
                        adapter.notifyCountChanged(entry.getKey(), 0);
                    }
                } else {
                    for (TaskGroup count : taskGroups) {
                        adapter.notifyCountChanged(count.pkgName, count.count);
                    }
                }
            }
        });

        super.onCreateOptionsMenu(menu, inflater);
    }
}