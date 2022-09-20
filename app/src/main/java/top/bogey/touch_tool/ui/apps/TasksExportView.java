package top.bogey.touch_tool.ui.apps;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.FileProvider;

import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.google.android.material.checkbox.MaterialCheckBox;
import com.google.gson.Gson;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.SheetTasksExportBinding;
import top.bogey.touch_tool.room.bean.Task;

public class TasksExportView extends BottomSheetDialogFragment {
    private static final String SAVE_FILE = "Share";
    private SheetTasksExportBinding binding;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = SheetTasksExportBinding.inflate(inflater);
        TasksExportRecyclerViewAdapter adapter = new TasksExportRecyclerViewAdapter(this);
        binding.tasksRecycleView.setAdapter(adapter);

        binding.selectAll.setOnClickListener(v -> adapter.selectAll(binding.selectAll.isChecked()));

        binding.exportButton.setOnClickListener(v -> {
            List<Task> selectTasks = adapter.selectTasks;
            String json = new Gson().toJson(selectTasks);

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
        });

        return binding.getRoot();
    }

    public void refreshSelectAllBox(int checkState){
        binding.selectAll.setCheckedState(checkState);
    }
}
