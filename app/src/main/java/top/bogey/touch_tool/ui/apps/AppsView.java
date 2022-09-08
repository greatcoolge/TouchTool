package top.bogey.touch_tool.ui.apps;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
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
import androidx.core.content.FileProvider;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.google.gson.Gson;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;

import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewAppsBinding;
import top.bogey.touch_tool.room.bean.Task;

public class AppsView extends Fragment {
    private static final String SAVE_FILE = "Share.txt";

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

                MenuItem showSystemItem = menu.findItem(R.id.show_system);
                showSystemItem.setChecked(Boolean.TRUE.equals(viewModel.showSystem.getValue()));
            }

            @SuppressLint("NonConstantResourceId")
            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                List<Task> tasks = viewModel.getAllTasks();
                switch (menuItem.getItemId()) {
                    case R.id.show_system:
                        viewModel.showSystem.setValue(!Boolean.TRUE.equals(viewModel.showSystem.getValue()));
                        menuItem.setChecked(Boolean.TRUE.equals(viewModel.showSystem.getValue()));
                        viewModel.refreshAppList();
                        adapter.refreshApps(viewModel.searchAppList(searchText));
                        return true;
                    case R.id.export_actions:
                        String json = new Gson().toJson(tasks);

                        try(FileOutputStream fileOutputStream = requireContext().openFileOutput(SAVE_FILE, Context.MODE_PRIVATE)){
                            fileOutputStream.write(json.getBytes());
                        } catch (IOException e) {
                            e.printStackTrace();
                        }

                        Intent intent = new Intent(Intent.ACTION_SEND);
                        File file = new File(requireContext().getFilesDir(), SAVE_FILE);
                        Uri fileUri = null;
                        try {
                            fileUri = FileProvider.getUriForFile(requireContext(), requireContext().getPackageName() + ".file_provider", file);
                        } catch (IllegalArgumentException ignored){}
                        if (fileUri != null){
                            intent.putExtra(Intent.EXTRA_STREAM, fileUri);
                            String type = requireContext().getContentResolver().getType(fileUri);
                            intent.setType(type);
                            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_GRANT_READ_URI_PERMISSION);
                            requireContext().startActivity(Intent.createChooser(intent, getString(R.string.export_tips)));
                        }
                        return true;
                    case R.id.clean_actions:
                        List<String> pkgNames = viewModel.getAllPkgNames();
                        for (Task task : tasks) {
                            if (!pkgNames.contains(task.getPkgName())){
                                viewModel.deleteTask(task);
                            }
                        }
                        Toast.makeText(requireContext(), R.string.clean_tips, Toast.LENGTH_LONG).show();
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
