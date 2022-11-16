package top.bogey.touch_tool.ui.task;

import android.os.Bundle;
import android.text.Editable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.textfield.TextInputEditText;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ViewTaskTabBinding;

public class TagView extends BottomSheetDialogFragment {
    private final TaskView parent;

    public TagView(TaskView parent) {
        this.parent = parent;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        ViewTaskTabBinding binding = ViewTaskTabBinding.inflate(inflater, container, false);

        TagRecyclerViewAdapter adapter = new TagRecyclerViewAdapter(parent);
        binding.appBox.setAdapter(adapter);

        binding.addButton.setOnClickListener(v -> {
            View view = LayoutInflater.from(requireContext()).inflate(R.layout.widget_text_input, null);
            TextInputEditText editText = view.findViewById(R.id.title_edit);

            new MaterialAlertDialogBuilder(requireContext())
                    .setPositiveButton(R.string.enter, (dialog, which) -> {
                        Editable text = editText.getText();
                        if (text != null) {
                            TaskRepository.getInstance().addTag(requireContext(), String.valueOf(text));
                            adapter.addTag(String.valueOf(text));
                        }
                        dialog.dismiss();
                    })
                    .setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss())
                    .setView(view)
                    .setTitle(R.string.tag_add)
                    .show();
        });

        return binding.getRoot();
    }
}
