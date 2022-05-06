package top.bogey.auto_touch.ui.tasks;

import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.text.Editable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.ToggleButton;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentActionsItemBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.action.FloatActionEdit;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class ActionsRecyclerViewAdapter extends RecyclerView.Adapter<ActionsRecyclerViewAdapter.ViewHolder> {
    private final TasksFragment parent;
    private final MainViewModel viewModel;
    private List<Action> actions = new ArrayList<>();
    private Task task;

    public ActionsRecyclerViewAdapter(TasksFragment parent){
        this.parent = parent;
        viewModel = new ViewModelProvider(parent.requireActivity()).get(MainViewModel.class);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new ViewHolder(FragmentActionsItemBinding.inflate(LayoutInflater.from(parent.getContext()), parent, false));
    }

    @Override
    public void onBindViewHolder(@NonNull final ViewHolder holder, int position) {
        if (actions.size() == 1) {
            holder.layout.setBackgroundResource(R.drawable.item_a);
            holder.up.setVisibility(View.GONE);
            holder.down.setVisibility(View.GONE);
        } else if (position == 0) {
            holder.layout.setBackgroundResource(R.drawable.item_f);
            holder.up.setVisibility(View.GONE);
            holder.down.setVisibility(View.VISIBLE);
        } else if (position == actions.size() - 1) {
            holder.layout.setBackgroundResource(R.drawable.item_l);
            holder.up.setVisibility(View.VISIBLE);
            holder.down.setVisibility(View.GONE);
        } else {
            holder.layout.setBackgroundResource(R.drawable.item_m);
            holder.up.setVisibility(View.VISIBLE);
            holder.down.setVisibility(View.VISIBLE);
        }
        Action action = actions.get(position);
        String title = action.getTitle();
        if (title.isEmpty()){
            holder.title.setText(action.getDefaultTitle(parent.requireContext()));
        } else {
            holder.title.setText(title);
        }
        holder.enabledToggle.setChecked(action.isEnable());
        holder.refreshSelectState(action.isEnable());
        holder.enabledToggle.setText(String.valueOf(position + 1));
        switch (action.getActionMode()) {
            case CONDITION:
                holder.modeImage.setImageResource(R.drawable.condition);
                break;
            case LOOP:
                holder.modeImage.setImageResource(R.drawable.loop);
                break;
            case PARALLEL:
                holder.modeImage.setImageResource(R.drawable.parallel);
                break;
        }
    }

    @Override
    public int getItemCount() {
        return actions.size();
    }

    public void setTask(Task task){
        this.task = task;
        if (task.getActions() == null) task.setActions(new ArrayList<>());
        actions = task.getActions();
        notifyDataSetChanged();
    }

    public void notifyNew(){
        notifyItemInserted(actions.size() - 1);
        notifyItemChanged(actions.size() - 2);
    }

    protected class ViewHolder extends RecyclerView.ViewHolder {
        public final ConstraintLayout layout;
        public final ToggleButton enabledToggle;
        public final TextView title;
        public final EditText titleEdit;
        public final Button up;
        public final Button down;
        public final Button delete;
        public final ImageView modeImage;


        public ViewHolder(FragmentActionsItemBinding binding) {
            super(binding.getRoot());
            layout = binding.getRoot();
            enabledToggle = binding.enabledToggle;
            title = binding.titleText;
            titleEdit = binding.titleEdit;
            up = binding.upButton;
            down = binding.downButton;
            delete = binding.deleteButton;
            modeImage = binding.modeImage;

            layout.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                new FloatActionEdit(parent.requireContext(), task, action, () -> {
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                }).show();
            });

            title.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                new FloatActionEdit(parent.requireContext(), task, action, () -> {
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                }).show();
            });

            title.setOnLongClickListener(v -> {
                titleEdit.setVisibility(View.VISIBLE);
                title.setVisibility(View.GONE);
                titleEdit.setText(title.getText());
                titleEdit.requestFocus();
                titleEdit.selectAll();
                InputMethodManager manager = (InputMethodManager) parent.requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
                if (manager != null) manager.showSoftInput(titleEdit, InputMethodManager.SHOW_IMPLICIT);
                return true;
            });

            titleEdit.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)){
                    Editable text = titleEdit.getText();
                    String titleStr = "";
                    if (text != null && text.length() > 0){
                        titleStr = text.toString();
                    }
                    int index = getBindingAdapterPosition();
                    Action action = actions.get(index);
                    action.setTitle(titleStr);
                    notifyItemChanged(index);
                    viewModel.saveTask(task);
                    titleEdit.setVisibility(View.GONE);
                    title.setVisibility(View.VISIBLE);
                }
                return true;
            });

            enabledToggle.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                Action action = actions.get(index);
                action.setEnable(!action.isEnable());
                refreshSelectState(action.isEnable());
                viewModel.saveTask(task);
            });

            up.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.max(0, index - 1);
                actions.add(newIndex, actions.remove(index));
                notifyItemRangeChanged(newIndex, 2);
                viewModel.saveTask(task);
            });

            down.setOnClickListener(v -> {
                int index = getBindingAdapterPosition();
                int newIndex = Math.min(actions.size() - 1, index + 1);
                actions.add(newIndex, actions.remove(index));
                notifyItemRangeChanged(index, 2);
                viewModel.saveTask(task);
            });

            delete.setOnClickListener(v -> AppUtil.showSimpleDialog(parent.requireActivity(), R.string.delete_action_tips, new SelectCallback() {
                @Override
                public void onEnter() {
                    int index = getBindingAdapterPosition();
                    actions.remove(index);
                    notifyItemRemoved(index);
                    if (!actions.isEmpty()){
                        if (index == 0){
                            notifyItemChanged(0);
                        } else if (index == actions.size()){
                            notifyItemChanged(index - 1);
                        } else {
                            notifyItemChanged(index - 1);
                            notifyItemChanged(index);
                        }
                    }
                    viewModel.saveTask(task);
                }

                @Override
                public void onCancel() { }
            }));
        }

        public void refreshSelectState(boolean isChecked){
            Context context = parent.requireContext();
            ColorStateList colorStateList;
            if (isChecked){
                colorStateList = ColorStateList.valueOf(AppUtil.getGroupColor(context, task.getGroupId()));
            } else {
                int[] attrs = new int[] {R.attr.backgroundColor};
                TypedArray typedArray = context.getTheme().obtainStyledAttributes(attrs);
                int selectColor = typedArray.getResourceId(0, R.color.grey_300);
                typedArray.recycle();
                colorStateList = ColorStateList.valueOf(context.getResources().getColor(selectColor, null));
            }
            enabledToggle.setBackgroundTintList(colorStateList);
            modeImage.setImageTintList(colorStateList);
        }
    }
}