package top.bogey.auto_touch.ui.tasks;

import android.content.Context;
import android.text.Editable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.RadioGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentTasksItemBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.TaskStatus;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class TasksRecyclerViewAdapter extends RecyclerView.Adapter<TasksRecyclerViewAdapter.ViewHolder> {
    private final TasksFragment parent;
    private final MainViewModel viewModel;
    private final List<Task> tasks = new ArrayList<>();

    public TasksRecyclerViewAdapter(TasksFragment parent){
        this.parent = parent;
        viewModel = new ViewModelProvider(parent.requireActivity()).get(MainViewModel.class);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FragmentTasksItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, int position) {
        Task task = tasks.get(position);
        View child = holder.group.getChildAt(task.taskStatus.ordinal());
        holder.group.check(child.getId());
        holder.title.setText(task.title);
        holder.title.setTextColor(AppUtil.getGroupColor(parent.requireContext(), task.groupId));
        holder.number.setText(String.valueOf(task.actions != null ? task.actions.size() : 0));

        for (int i = 0; i < holder.groups.length; i++) {
            if (i == task.groupId - 1){
                holder.groups[i].setAlpha(1f);
            } else {
                holder.groups[i].setAlpha(0.04f);
            }
        }
    }

    @Override
    public int getItemCount() {
        return tasks.size();
    }

    public void setTasks(List<Task> newTasks){
        if (newTasks == null){
            int size = tasks.size();
            tasks.clear();
            notifyItemRangeRemoved(0, size);
            return;
        }
        // 查找删除的 或 动作变更了的
        for (int i = tasks.size() - 1; i >= 0; i--) {
            Task task = tasks.get(i);
            boolean flag = true;
            for (Task newTask : newTasks) {
                if (task.id == newTask.id) {
                    flag = false;
                    break;
                }
            }
            if (flag){
                tasks.remove(i);
                notifyItemRemoved(i);
            }
        }

        // 查找新增的
        for (Task newTask : newTasks) {
            boolean flag = true;
            for (Task task : tasks) {
                if (task.id == newTask.id){
                    flag = false;
                    break;
                }
            }
            if (flag){
                tasks.add(newTask);
                notifyItemInserted(tasks.size() - 1);
            }
        }
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final ConstraintLayout layout;
        public final RadioGroup group;
        public final TextView title;
        public final EditText titleEdit;
        public final TextView number;
        public final Button delete;
        public final LinearLayout linearLayout;
        public final Button[] groups = new Button[3];


        public ViewHolder(FragmentTasksItemBinding binding) {
            super(binding.getRoot());
            layout = binding.getRoot();
            group = binding.statusGroup;
            title = binding.title;
            number = binding.numberText;
            titleEdit = binding.titleEdit;
            delete = binding.delete;
            linearLayout = binding.linearLayout;
            groups[0] = binding.group1;
            groups[1] = binding.group2;
            groups[2] = binding.group3;

            layout.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Task task = tasks.get(index);
                NavController controller = Navigation.findNavController(parent.requireActivity(), R.id.con_view);
                controller.navigate(TasksFragmentDirections.actionTasksFragmentToActionsFragment(task));
            });

            layout.setOnLongClickListener(v -> {
                title.setVisibility(View.INVISIBLE);
                titleEdit.setVisibility(View.VISIBLE);
                titleEdit.setText(title.getText());
                titleEdit.setTextColor(title.getCurrentTextColor());
                titleEdit.requestFocus();
                InputMethodManager manager = (InputMethodManager) parent.requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
                if (manager != null) manager.showSoftInput(titleEdit, 0);
                return true;
            });

            titleEdit.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)){
                    Editable text = titleEdit.getText();
                    if (text != null && text.length() > 0){
                        int index = getAdapterPosition();
                        Task task = tasks.get(index);
                        title.setText(text);
                        task.title = text.toString();
                        notifyItemChanged(index);
                        viewModel.saveTask(task);
                    }
                    titleEdit.setVisibility(View.INVISIBLE);
                    title.setVisibility(View.VISIBLE);
                }
                return true;
            });

            group.setOnCheckedChangeListener((group, checkedId) -> {
                int selectIndex = group.indexOfChild(group.findViewById(checkedId));
                int index = getAdapterPosition();
                Task task = tasks.get(index);
                task.taskStatus = TaskStatus.values()[selectIndex];
                viewModel.saveTask(task);
            });

            delete.setOnClickListener(v -> {
                int index = getAdapterPosition();
                Task task = tasks.get(index);
                AppUtil.showSimpleDialog(parent.requireActivity(), parent.requireContext().getString(R.string.delete_task_tips, task.title), new SelectCallback() {
                    @Override
                    public void onEnter() {
                        tasks.remove(index);
                        notifyItemRemoved(index);
                        viewModel.deleteTask(task);
                    }

                    @Override
                    public void onCancel() { }
                });
            });

            for (Button button : groups) {
                button.setOnClickListener(v -> {
                    int childIndex = linearLayout.indexOfChild((View) v.getParent());
                    int index = getAdapterPosition();
                    Task task = tasks.get(index);
                    if (task.groupId == childIndex){
                        task.groupId = 0;
                    } else {
                        task.groupId = childIndex;
                    }
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                });
            }
        }
    }
}