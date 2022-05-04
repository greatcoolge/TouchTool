package top.bogey.auto_touch;

import android.content.Context;
import android.content.Intent;
import android.media.projection.MediaProjectionManager;
import android.net.Uri;
import android.os.Bundle;
import android.os.ParcelFileDescriptor;
import android.provider.Settings;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.navigation.ui.AppBarConfiguration;
import androidx.navigation.ui.NavigationUI;

import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import com.google.gson.reflect.TypeToken;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import top.bogey.auto_touch.databinding.ActivityMainBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.play.TaskPlayerDialog;
import top.bogey.auto_touch.util.PermissionResultCallback;

public class MainActivity extends AppCompatActivity {
    static { System.loadLibrary("auto_touch"); }

    private ActivityMainBinding binding;
    private ActivityResultLauncher<Intent> captureLauncher;
    private PermissionResultCallback captureCallback;

    private ActivityResultLauncher<Intent> floatLauncher;
    private PermissionResultCallback floatCallback;

    private TaskPlayerDialog playerDialog;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        MainApplication.setActivity(this);

        captureLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
            if (captureCallback != null){
                captureCallback.onResult(result.getResultCode(), result.getData());
            }
        });

        floatLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
            if (floatCallback != null){
                floatCallback.onResult(result.getResultCode(), result.getData());
            }
        });

        binding.getRoot().post(() -> handleIntent(getIntent()));
    }

    @Override
    protected void onStart() {
        super.onStart();
        AppBarConfiguration configuration = new AppBarConfiguration.Builder(R.id.home_fragment, R.id.apps_fragment, R.id.setting_fragment).build();
        NavController controller = Navigation.findNavController(this, R.id.con_view);
        NavigationUI.setupActionBarWithNavController(this, controller, configuration);
        NavigationUI.setupWithNavController(binding.menuMain, controller);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        MainApplication.setActivity(null);
    }

    @Override
    public boolean onSupportNavigateUp() {
        NavController controller = Navigation.findNavController(this, R.id.con_view);
        return controller.navigateUp() || super.onSupportNavigateUp();
    }

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        handleIntent(intent);
    }

    private void handleIntent(Intent intent){
        boolean isBackground = intent.getBooleanExtra("IsBackground", false);
        if (isBackground){
            moveTaskToBack(true);
        }

        String pkgName = getIntent().getStringExtra("FloatPackageName");
        if (pkgName != null && !pkgName.isEmpty()){
            showPlayView(pkgName);
        }

        if (Intent.ACTION_SEND.equals(intent.getAction()) && intent.getType() != null){
            if ("text/plain".equals(intent.getType())){
                Uri uri = intent.getParcelableExtra(Intent.EXTRA_STREAM);
                if (uri != null){
                    ParcelFileDescriptor fileDescriptor = null;
                    try {
                        fileDescriptor = getContentResolver().openFileDescriptor(uri, "r");
                    } catch (FileNotFoundException ignored){}
                    if (fileDescriptor != null){
                        FileDescriptor descriptor = fileDescriptor.getFileDescriptor();

                        StringBuilder buffer = new StringBuilder();
                        try(FileInputStream fileInputStream = new FileInputStream(descriptor)){
                            InputStreamReader reader = new InputStreamReader(fileInputStream, StandardCharsets.UTF_8);
                            int i = reader.read();
                            while (i != -1){
                                char c = (char) i;
                                buffer.append(c);
                                i = reader.read();
                            }
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                        saveTasks(buffer.toString());

                        try {
                            fileDescriptor.close();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    }

    public void saveTasks(String tasksString){
        if (tasksString == null || tasksString.isEmpty()) return;

        MainViewModel viewModel = new ViewModelProvider(this).get(MainViewModel.class);
        List<String> pkgNames = viewModel.getAllPkgNames();

        List<Task> tasks = null;
        try {
            tasks = new Gson().fromJson(tasksString, new TypeToken<List<Task>>() {}.getType());
        } catch (JsonParseException ignored){}
        if (tasks != null){
            List<Task> newTasks = new ArrayList<>();
            for (Task task : tasks) {
                if (pkgNames.contains(task.getPkgName())){
                    if (task.getActions() != null && !task.getActions().isEmpty()){
                        newTasks.add(task);
                    }
                }
            }
            viewModel.saveTask(newTasks);
        }
    }

    public void launchCapture(PermissionResultCallback callback){
        captureCallback = callback;
        MediaProjectionManager manager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
        captureLauncher.launch(manager.createScreenCaptureIntent());
    }

    public void launchFloat(PermissionResultCallback callback){
        floatCallback = callback;
        try {
            Field field = Settings.class.getDeclaredField("ACTION_MANAGE_OVERLAY_PERMISSION");
            Intent intent = new Intent(Objects.requireNonNull(field.get(null)).toString());
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setData(Uri.parse("package:" + getPackageName()));
            floatLauncher.launch(intent);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void showPlayView(String pkgName){
        binding.getRoot().post(() -> {
            if (playerDialog != null) playerDialog.dismiss();
            playerDialog = new TaskPlayerDialog(this, pkgName, () -> playerDialog = null);
            playerDialog.show(0, 0);
        });
    }

    public void dismissPlayView() {
        binding.getRoot().post(() -> {
            if (playerDialog != null) playerDialog.dismiss();
        });
    }
}

