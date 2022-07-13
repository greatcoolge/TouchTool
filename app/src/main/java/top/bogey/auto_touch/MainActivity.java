package top.bogey.auto_touch;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.navigation.ui.AppBarConfiguration;
import androidx.navigation.ui.NavigationUI;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.media.projection.MediaProjectionManager;
import android.net.Uri;
import android.os.Bundle;
import android.os.ParcelFileDescriptor;
import android.provider.Settings;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;
import com.google.gson.reflect.TypeToken;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import top.bogey.auto_touch.databinding.ActivityMainBinding;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.room.bean.node.Node;
import top.bogey.auto_touch.room.data.CustomTypeConverts;
import top.bogey.auto_touch.ui.play.PlayFloatView;
import top.bogey.auto_touch.utils.PermissionResultCallback;
import top.bogey.auto_touch.utils.easy_float.EasyFloat;

public class MainActivity extends AppCompatActivity {
    static {System.loadLibrary("auto_touch");}

    private static final String SAVE_PATH = "Save";
    private static final String FIRST_RUN = "first_run";

    private ActivityMainBinding binding;

    private ActivityResultLauncher<Intent> captureLauncher;
    private ActivityResultLauncher<Intent> floatLauncher;
    private PermissionResultCallback resultCallback;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        setSupportActionBar(binding.toolBar);

        MainApplication.setActivity(this);

        captureLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
            if (resultCallback != null){
                resultCallback.onResult(result.getResultCode(), result.getData());
            }
        });

        floatLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
            if (resultCallback != null){
                resultCallback.onResult(result.getResultCode(), result.getData());
            }
        });

        binding.getRoot().post(() -> handleIntent(getIntent()));

        runFirstTimes();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        MainApplication.setActivity(null);
    }

    @Override
    protected void onStart() {
        super.onStart();
        NavController controller = Navigation.findNavController(this, R.id.con_view);
        NavigationUI.setupWithNavController(binding.menuView, controller);
        AppBarConfiguration configuration = new AppBarConfiguration.Builder(R.id.home, R.id.apps, R.id.setting).build();
        NavigationUI.setupActionBarWithNavController(this, controller, configuration);
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

    private void runFirstTimes(){
        SharedPreferences preferences = getSharedPreferences(SAVE_PATH, Context.MODE_PRIVATE);
        boolean firstRun = preferences.getBoolean(FIRST_RUN, false);
        if (!firstRun){
            StringBuilder buffer = new StringBuilder();
            try {
                InputStream inputStream = getAssets().open("DefaultTasks");
                InputStreamReader reader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
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

            SharedPreferences.Editor edit = preferences.edit();
            edit.putBoolean(FIRST_RUN, true);
            edit.apply();
        }
    }

    public void handleIntent(Intent intent){
        boolean isBackground = intent.getBooleanExtra("IsBackground", false);
        if (isBackground){
            moveTaskToBack(true);
        }

        String pkgName = getIntent().getStringExtra("FloatPackageName");
        if (pkgName != null && !pkgName.isEmpty()){
            showPlayFloatView(pkgName);
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
        Gson gson = new GsonBuilder().registerTypeAdapter(Node.class, new CustomTypeConverts.NodeAdapter()).create();
        try {
            tasks = gson.fromJson(tasksString, new TypeToken<List<Task>>(){}.getType());
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
        resultCallback = callback;
        MediaProjectionManager manager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
        captureLauncher.launch(manager.createScreenCaptureIntent());
    }

    public void launchFloat(PermissionResultCallback callback){
        resultCallback = callback;
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

    public void showPlayFloatView(String pkgName){
        binding.getRoot().post(() -> new PlayFloatView(this, pkgName).show());
    }

    public void dismissPlayFloatView(){
        binding.getRoot().post(() -> EasyFloat.dismiss(PlayFloatView.class.getCanonicalName()));
    }
}