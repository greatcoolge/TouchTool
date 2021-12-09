package top.bogey.auto_touch;

import android.content.Context;
import android.content.Intent;
import android.media.projection.MediaProjectionManager;
import android.net.Uri;
import android.os.Bundle;
import android.os.ParcelFileDescriptor;
import android.view.Gravity;

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

import java.io.BufferedReader;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.databinding.ActivityMainBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.ui.play.TaskPlayerDialog;
import top.bogey.auto_touch.util.PermissionResultCallback;

public class MainActivity extends AppCompatActivity {
    static { System.loadLibrary("auto_touch"); }

    private ActivityMainBinding binding;
    private ActivityResultLauncher<Intent> captureLauncher;
    private PermissionResultCallback callback;
    private Intent captureIntent;

    private TaskPlayerDialog playerDialog;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        MainApplication.setActivity(this);

        startService(new Intent(this, MainAccessibilityService.class));

        captureLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
            if (result.getResultCode() == RESULT_OK){
                captureIntent = result.getData();
                if (callback != null){
                    callback.onResult(result.getResultCode(), captureIntent);
                }
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        AppBarConfiguration configuration = new AppBarConfiguration.Builder(R.id.home_fragment, R.id.apps_fragment, R.id.setting_fragment).build();
        NavController controller = Navigation.findNavController(this, R.id.con_view);
        NavigationUI.setupActionBarWithNavController(this, controller, configuration);
        NavigationUI.setupWithNavController(binding.menuMain, controller);

        handleIntent(getIntent());
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
                if (pkgNames.contains(task.pkgName)){
                    if (task.actions != null && !task.actions.isEmpty()){
                        boolean flag = true;
                        for (Action action : task.actions) {
                            if (!action.checkTimeSafe()){
                                flag = false;
                                break;
                            }
                        }
                        if (flag){
                            task.id = 0;
                            newTasks.add(task);
                        }
                    }
                }
            }
            viewModel.saveTask(newTasks);
        }
    }

    public void launcherCapture(PermissionResultCallback callback){
        if (captureIntent != null){
            callback.onResult(RESULT_OK, captureIntent);
            return;
        }
        this.callback = callback;
        MediaProjectionManager manager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
        captureLauncher.launch(manager.createScreenCaptureIntent());
    }

    public void showPlayView(String pkgName){
        binding.getRoot().post(() -> {
            if (playerDialog != null) playerDialog.dismiss();
            playerDialog = new TaskPlayerDialog(this, pkgName, () -> playerDialog = null);
            playerDialog.show(Gravity.END | Gravity.CENTER_VERTICAL, 0, 0);
        });
    }

    public void dismissPlayView() {
        binding.getRoot().post(() -> {
            if (playerDialog != null) playerDialog.dismiss();
        });
    }
}

