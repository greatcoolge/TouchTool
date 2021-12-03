package top.bogey.auto_touch.ui.actions;

import android.content.Context;
import android.os.Bundle;
import android.text.Editable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.EditText;
import android.widget.RadioGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.ViewModelProvider;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentActionsBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.action.ActionEditDialog;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class ActionsFragment extends Fragment {
    private FragmentActionsBinding binding;
    private MainViewModel viewModel;
    private Task task;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentActionsBinding.inflate(inflater, container, false);
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);

        if (getArguments() != null){
            int taskId = getArguments().getInt("taskId");
            task = viewModel.getTasksById(taskId);
            if (task != null){
                ActionsRecyclerViewAdapter adapter = new ActionsRecyclerViewAdapter(this, task);
                binding.recyclerView.setAdapter(adapter);

                initTaskInfo();
                LiveData<List<Task>> liveData = viewModel.getTasksLiveById(taskId);
                liveData.observe(getViewLifecycleOwner(), this::refreshTaskInfo);
            }
        }
        return binding.getRoot();
    }

    private void initTaskInfo(){
        TextView title = binding.include.title;
        EditText titleEdit = binding.include.titleEdit;
        RadioGroup group = binding.include.statusGroup;
        Button delete = binding.include.delete;

        binding.include.getRoot().setOnLongClickListener(v -> {
            title.setVisibility(View.INVISIBLE);
            titleEdit.setVisibility(View.VISIBLE);
            titleEdit.setText(title.getText());
            titleEdit.setTextColor(title.getCurrentTextColor());
            titleEdit.requestFocus();
            InputMethodManager manager = (InputMethodManager) requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
            if (manager != null) manager.showSoftInput(titleEdit, 0);
            return true;
        });

        titleEdit.setOnEditorActionListener((v, actionId, event) -> {
            if (actionId == EditorInfo.IME_ACTION_DONE || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)){
                Editable text = titleEdit.getText();
                if (text != null && text.length() > 0){
                    title.setText(text);
                    task.title = text.toString();
                    viewModel.saveTask(task);
                }
                titleEdit.setVisibility(View.INVISIBLE);
                title.setVisibility(View.VISIBLE);
            }
            return true;
        });

        group.setOnCheckedChangeListener((view, checkedId) -> {
            int selectIndex = view.indexOfChild(view.findViewById(checkedId));
            task.taskStatus = TaskStatus.values()[selectIndex];
            viewModel.saveTask(task);
        });

        delete.setOnClickListener(v -> AppUtil.showSimpleDialog(requireActivity(), requireContext().getString(R.string.delete_task_tips, task.title), new SelectCallback() {
            @Override
            public void onEnter() {
                requireActivity().onBackPressed();
                viewModel.deleteTask(task);
            }

            @Override
            public void onCancel() { }
        }));

        Button[] groups = {binding.include.group1, binding.include.group2, binding.include.group3};
        for (Button button : groups) {
            button.setOnClickListener(v -> {
                int childIndex = binding.include.linearLayout.indexOfChild((View) v.getParent());
                if (task.groupId == childIndex){
                    task.groupId = 0;
                } else {
                    task.groupId = childIndex;
                }
                viewModel.saveTask(task);
            });
        }
    }

    private void refreshTaskInfo(List<Task> tasks){
        if (tasks != null && !tasks.isEmpty()){
            task = tasks.get(0);

            ActionsRecyclerViewAdapter adapter = (ActionsRecyclerViewAdapter) binding.recyclerView.getAdapter();
            if (adapter != null){
                adapter.setActions(task);
            }

            RadioGroup group = binding.include.statusGroup;
            group.check(group.getChildAt(task.taskStatus.ordinal()).getId());

            binding.include.title.setText(task.title);
            binding.include.title.setTextColor(AppUtil.getGroupColor(requireContext(), task.groupId));

            binding.include.numberText.setText(String.valueOf(task.actions != null ? task.actions.size() : 0));

            Button[] groups = {binding.include.group1, binding.include.group2, binding.include.group3};
            for (int i = 0; i < groups.length; i++) {
                if (i == task.groupId - 1){
                    groups[i].setAlpha(1f);
                } else {
                    groups[i].setAlpha(0.04f);
                }
            }
        }
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
    }

    @Override
    public void onCreateOptionsMenu(@NonNull Menu menu, @NonNull MenuInflater inflater) {
        inflater.inflate(R.menu.menu_actions, menu);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (item.getItemId() == R.id.add) {
            Action action = new Action();
            new ActionEditDialog(requireContext(), task, action, () -> {
                if (task.actions == null) task.actions = new ArrayList<>();
                task.actions.add(action);
                viewModel.saveTask(task);
            }).show();
        }
        return super.onOptionsItemSelected(item);
    }
}
