package top.bogey.auto_touch.ui.setting;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.core.content.FileProvider;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.google.gson.Gson;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;

import top.bogey.auto_touch.MainViewModel;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.ViewSettingBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.debug.DebugFloatView;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;

public class SettingView extends Fragment {
    private static final String SAVE_FILE = "Share.txt";

    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        ViewSettingBinding binding = ViewSettingBinding.inflate(inflater, container, false);
        MainViewModel viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);

        binding.exportButton.setOnClickListener(v -> {
            List<Task> tasks = viewModel.getAllTasks();
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
        });

        binding.cleanButton.setOnClickListener(v -> {
            List<String> pkgNames = viewModel.getAllPkgNames();
            List<Task> tasks = viewModel.getAllTasks();
            for (Task task : tasks) {
                if (!pkgNames.contains(task.getPkgName())){
                    viewModel.deleteTask(task);
                }
            }
            Toast.makeText(requireContext(), R.string.clean_tips, Toast.LENGTH_LONG).show();
        });

        binding.zanButton.setOnClickListener(v -> {
            try {
                String address = "market://details?id=" + requireContext().getPackageName();
                Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(address));
                intent.setPackage("com.coolapk.market");
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                startActivity(intent);
            } catch (Exception ignored){
                Toast.makeText(requireContext(), R.string.market_tips, Toast.LENGTH_LONG).show();
            }
        });

        binding.bookButton.setOnClickListener(v -> {
            try {
                Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse("https://github.com/mr-bogey/AutoTouch"));
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                startActivity(intent);
            } catch (Exception ignored){ }
        });

        binding.bookButton.setOnLongClickListener(v -> {
            View view = EasyFloat.getView(DebugFloatView.class.getCanonicalName());
            if (view != null) ((DebugFloatView) view).dismiss();
            else new DebugFloatView(requireContext()).show();
            return true;
        });

        return binding.getRoot();
    }
}
