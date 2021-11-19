package top.bogey.auto_touch.ui.actions;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.RadioGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentActionsBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.util.AppUtil;

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
            task = (Task)getArguments().getSerializable("task");
            if (task != null){
                binding.recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));
                ActionsRecyclerViewAdapter adapter = new ActionsRecyclerViewAdapter(this, task);
                binding.recyclerView.setAdapter(adapter);

                RadioGroup group = binding.include.statusGroup;
                group.check(group.getChildAt(task.taskStatus.ordinal()).getId());

                binding.include.title.setText(task.title);
                binding.include.title.setTextColor(AppUtil.getGroupColor(requireContext(), task.groupId));

                updateActionCount();

                Button[] groups = {binding.include.group1, binding.include.group2, binding.include.group3};
                for (int i = 0; i < groups.length; i++) {
                    if (i == task.groupId - 1){
                        groups[i].setAlpha(1f);
                    } else {
                        groups[i].setAlpha(0.04f);
                    }
                }

                binding.include.delete.setVisibility(View.INVISIBLE);
                binding.include.closedButton.setEnabled(false);
                binding.include.autoButton.setEnabled(false);
                binding.include.manualButton.setEnabled(false);
            }
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

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (item.getItemId() == R.id.add) {
            if (task.actions == null) task.actions = new ArrayList<>();
            Action action = new Action();
            action.target = new Node("跳过");
            task.actions.add(action);
            viewModel.saveTask(task);
            ActionsRecyclerViewAdapter adapter = (ActionsRecyclerViewAdapter) binding.recyclerView.getAdapter();
            if (adapter != null){
                adapter.notifyItemInserted(adapter.getItemCount() - 1);
                adapter.notifyItemChanged(Math.max(0, adapter.getItemCount() - 2));
            }
            updateActionCount();
        }
        return super.onOptionsItemSelected(item);
    }

    public void updateActionCount(){
        binding.include.numberText.setText(String.valueOf(task.actions != null ? task.actions.size() : 0));
    }
}
