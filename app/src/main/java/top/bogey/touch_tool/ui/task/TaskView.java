package top.bogey.touch_tool.ui.task;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.os.Parcel;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.FileProvider;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.recyclerview.widget.StaggeredGridLayoutManager;

import com.google.android.material.tabs.TabLayout;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskType;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTaskBinding;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.SelectCallback;
import top.bogey.touch_tool.utils.TaskChangedCallback;

public class TaskView extends Fragment implements TaskChangedCallback {
    private static final String SAVE_FILE = "Share_T.txt";

    private ViewTaskBinding binding;
    private MainViewModel viewModel;
    private TaskRecyclerViewAdapter adapter;

    private List<String> tags = new ArrayList<>();

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        refreshSpawnCount();
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = ViewTaskBinding.inflate(inflater, container, false);

        binding.addButton.setOnClickListener(v -> {
            Task task = new Task(requireContext());
            TaskRepository.getInstance().saveTask(task);
            NavController controller = Navigation.findNavController(MainApplication.getActivity(), R.id.con_view);
            controller.navigate(TaskViewDirections.actionTaskToTaskInfo(task));
        });

        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
        binding.pasteButton.setOnClickListener(v -> {
            Task task = viewModel.getCopyTask();
            TaskRepository.getInstance().saveTask(task);
            viewModel.setCopyTask(null);
        });

        viewModel.copyTask.observe(getViewLifecycleOwner(), task -> {
            if (task == null) binding.pasteButton.hide();
            else binding.pasteButton.show();
        });

        adapter = new TaskRecyclerViewAdapter(this);
        binding.tasksBox.setAdapter(adapter);

        requireActivity().addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menuInflater.inflate(R.menu.menu_task, menu);
            }

            @SuppressLint("NonConstantResourceId")
            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                switch (menuItem.getItemId()) {
                    case R.id.import_tasks:
                        MainActivity activity = MainApplication.getActivity();
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
                    case R.id.create_time:
                        adapter.setSortType(SortType.CREATE_TIME_ASC);
                        break;
                    case R.id.modify_time:
                        adapter.setSortType(SortType.MODIFY_TIME_ASC);
                        break;
                    default:
                        return false;
                }
                return true;
            }
        }, getViewLifecycleOwner());

        binding.selectAllButton.setOnClickListener(v -> adapter.selectAll());
        binding.deleteButton.setOnClickListener(v -> AppUtils.showDialog(requireContext(), R.string.delete_task_tips, new SelectCallback() {
            @Override
            public void onEnter() {
                adapter.deleteSelectTasks();
                hideBottomBar();
            }
        }));
        binding.exportButton.setOnClickListener(v -> {
            List<Task> tasks = adapter.getSelectTasks();

            try (FileOutputStream fileOutputStream = requireContext().openFileOutput(SAVE_FILE, Context.MODE_PRIVATE)) {
                Parcel parcel = Parcel.obtain();
                parcel.writeTypedList(tasks);
                fileOutputStream.write(parcel.marshall());
                parcel.recycle();
            } catch (IOException e) {
                e.printStackTrace();
            }

            Intent intent = new Intent(Intent.ACTION_SEND);
            File file = new File(requireContext().getFilesDir(), SAVE_FILE);
            Uri fileUri = null;
            try {
                fileUri = FileProvider.getUriForFile(requireContext(), requireContext().getPackageName() + ".file_provider", file);
            } catch (IllegalArgumentException ignored) {
            }
            if (fileUri != null) {
                intent.putExtra(Intent.EXTRA_STREAM, fileUri);
                String type = requireContext().getContentResolver().getType(fileUri);
                intent.setType(type);
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_GRANT_READ_URI_PERMISSION);
                requireContext().startActivity(Intent.createChooser(intent, getString(R.string.export_task_tips)));
            }
            adapter.unSelectAll();
            hideBottomBar();
        });

        binding.moveButton.setOnClickListener(v -> showTabView());

        binding.cancelButton.setOnClickListener(v -> {
            adapter.unSelectAll();
            hideBottomBar();
        });

        tags = TaskRepository.getInstance().getTags(requireContext());
        for (int i = 0; i < tags.size(); i++) {
            String tag = tags.get(i);
            binding.tabBox.addTab(binding.tabBox.newTab().setText(tag));
        }
        binding.tabBox.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                adapter.showTasksByTag(String.valueOf(tab.getText()));
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });
        selectTab(0);

        binding.folderButton.setOnClickListener(v -> showTabView());

        binding.closeButton.setOnClickListener(v -> setSelectTaskType(TaskType.CLOSED));
        binding.manualButton.setOnClickListener(v -> setSelectTaskType(TaskType.MANUAL));
        binding.itIsTimeButton.setOnClickListener(v -> setSelectTaskType(TaskType.IT_IS_TIME));
        binding.newNotificationButton.setOnClickListener(v -> setSelectTaskType(TaskType.NEW_NOTIFICATION));
        binding.appChangedButton.setOnClickListener(v -> setSelectTaskType(TaskType.APP_CHANGED));
        binding.viewChangedButton.setOnClickListener(v -> setSelectTaskType(TaskType.VIEW_CHANGED));
        binding.contentChangedButton.setOnClickListener(v -> setSelectTaskType(TaskType.CONTENT_CHANGED));

        return binding.getRoot();
    }

    public void setSelectTaskType(TaskType type){
        TaskRepository repository = TaskRepository.getInstance();
        for (Task task : adapter.getSelectTasks()) {
            task.setType(type);
            repository.saveTask(task);
        }
        adapter.unSelectAll();
        hideBottomBar();
    }

    public void addTab(String tag) {
        tags.add(tags.size() - 1, tag);
        binding.tabBox.addTab(binding.tabBox.newTab().setText(tag), tags.indexOf(tag));
    }

    public void removeTab(int index) {
        binding.tabBox.removeTabAt(index);
    }

    public void selectTab(int index) {
        TabLayout.Tab tab = binding.tabBox.getTabAt(index);
        if (tab != null) {
            if (adapter.isCheck()) {
                for (Task task : adapter.getSelectTasks()) {
                    if (index == 0 || index == tags.size() - 1) TaskRepository.getInstance().setTaskConfig(task.getId(), null);
                    else TaskRepository.getInstance().setTaskConfig(task.getId(), String.valueOf(tab.getText()));
                }
                hideBottomBar();
                adapter.showTasksByTag(String.valueOf(tab.getText()));
                adapter.unSelectAll();
            }
            binding.tabBox.selectTab(tab);
        }
    }

    private void showTabView(){
        TagView tagView = new TagView(this);
        tagView.show(requireActivity().getSupportFragmentManager(), null);
    }

    public void showBottomBar() {
        MainApplication.getActivity().hideBottomNavigation();

        binding.addButton.hide();
        binding.pasteButton.hide();
        binding.bottomBar.setVisibility(View.VISIBLE);

        adapter.setCheck(true);
    }

    public void hideBottomBar() {
        MainApplication.getActivity().showBottomNavigation();

        binding.addButton.show();
        if (viewModel.getCopyTask() != null) binding.pasteButton.show();
        binding.bottomBar.setVisibility(View.GONE);

        adapter.setCheck(false);
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
