package top.bogey.touch_tool;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.media.projection.MediaProjectionManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Parcel;
import android.provider.Settings;
import android.view.View;
import android.view.WindowManager;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.navigation.ui.AppBarConfiguration;
import androidx.navigation.ui.NavigationUI;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.databinding.ActivityMainBinding;
import top.bogey.touch_tool.ui.play.PlayFloatView;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.LogUtils;
import top.bogey.touch_tool.ui.setting.SettingSave;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.PermissionResultCallback;
import top.bogey.touch_tool.utils.SelectCallback;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

public class MainActivity extends AppCompatActivity {
    static {
        System.loadLibrary("auto_touch");
    }

    private ActivityMainBinding binding;

    private ActivityResultLauncher<Intent> intentLauncher;
    private ActivityResultLauncher<String> permissionLauncher;
    private ActivityResultLauncher<String> contentLauncher;
    private PermissionResultCallback resultCallback;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
            WindowManager.LayoutParams params = getWindow().getAttributes();
            params.layoutInDisplayCutoutMode = WindowManager.LayoutParams.LAYOUT_IN_DISPLAY_CUTOUT_MODE_SHORT_EDGES;
            getWindow().setAttributes(params);
        }

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        setSupportActionBar(binding.toolBar);

        MainApplication.setActivity(this);

        DisplayUtils.initParams(this);

        intentLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
            if (resultCallback != null) {
                resultCallback.onResult(result.getResultCode(), result.getData());
            }
        });

        permissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), result -> {
            if (result && resultCallback != null) resultCallback.onResult(RESULT_OK, null);
        });

        contentLauncher = registerForActivityResult(new ActivityResultContracts.GetContent(), result -> {
            if (result != null && resultCallback != null) {
                Intent intent = new Intent();
                intent.setData(result);
                resultCallback.onResult(RESULT_OK, intent);
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
        AppBarConfiguration configuration = new AppBarConfiguration.Builder(R.id.home, R.id.task, R.id.setting).build();
        NavigationUI.setupActionBarWithNavController(this, controller, configuration);
        controller.addOnDestinationChangedListener((navController, navDestination, bundle) -> {
            int id = navDestination.getId();
            if (id == R.id.home || id == R.id.task || id == R.id.setting) {
                showBottomNavigation();
            } else {
                hideBottomNavigation();
            }
        });
        SettingSave.getInstance().init(this);
    }

    public void showBottomNavigation() {
        binding.menuView.setVisibility(View.VISIBLE);
    }

    public void hideBottomNavigation() {
        binding.menuView.setVisibility(View.GONE);
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

    private void runFirstTimes() {
        if (!SettingSave.getInstance().isFirstRun()) {
            try (InputStream inputStream = getAssets().open("DefaultTasks")) {
                byte[] bytes = new byte[inputStream.available()];
                if (inputStream.read(bytes) == -1) saveTasks(bytes);
            } catch (IOException e) {
                e.printStackTrace();
            }
            SettingSave.getInstance().setFirstRun();
        }
    }

    public void handleIntent(Intent intent) {
        String pkgName = getIntent().getStringExtra("Goto");
        if (pkgName != null && !pkgName.isEmpty()) {
            AppUtils.gotoApp(this, pkgName);
        }

        boolean isBackground = intent.getBooleanExtra("IsBackground", false);
        if (isBackground) {
            moveTaskToBack(true);
        }

        pkgName = getIntent().getStringExtra("FloatPackageName");
        if (pkgName != null && !pkgName.isEmpty()) {
            showPlayFloatView(pkgName);
        }

        boolean dismissFloat = intent.getBooleanExtra("DismissFloat", false);
        if (dismissFloat) {
            dismissPlayFloatView();
        }

        if (Intent.ACTION_SEND.equals(intent.getAction()) && intent.getType() != null) {
            if ("text/plain".equals(intent.getType())) {
                Uri uri = intent.getParcelableExtra(Intent.EXTRA_STREAM);
                if (uri != null) {
                    saveTasksByFile(uri);
                }
            }
        }
    }

    public void saveTasksByFile(Uri uri) {
        try (InputStream inputStream = getContentResolver().openInputStream(uri)) {
            byte[] bytes = new byte[inputStream.available()];
            int read = inputStream.read(bytes);
            if (read > 0)
                saveTasks(bytes);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void saveTasks(byte[] bytes) {
        if (bytes == null || bytes.length == 0) return;

        Parcel parcel = Parcel.obtain();
        parcel.unmarshall(bytes, 0, bytes.length);
        parcel.setDataPosition(0);
        List<Task> tasks = parcel.createTypedArrayList(Task.CREATOR);

        MainViewModel viewModel = new ViewModelProvider(this).get(MainViewModel.class);
        List<String> pkgNames = viewModel.getAllPkgNames();

        if (tasks != null) {
            for (Task task : tasks) {
                List<String> taskPkgNames = new ArrayList<>();
                for (String pkgName : task.getPkgNames()) {
                    if (pkgNames.contains(pkgName)) {
                        taskPkgNames.add(pkgName);
                    }
                }
                // 判断任务需要的包是否存在
                if (taskPkgNames.size() > 0) {
                    task.setPkgNames(taskPkgNames);
                    TaskRepository.getInstance().saveTask(task);
                }
            }
        }
        parcel.recycle();
    }

    public void launchCapture(PermissionResultCallback callback) {
        resultCallback = callback;
        MediaProjectionManager manager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
        intentLauncher.launch(manager.createScreenCaptureIntent());
    }

    public void launchFloat(PermissionResultCallback callback) {
        resultCallback = callback;
        Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.setData(Uri.parse("package:" + getPackageName()));
        intentLauncher.launch(intent);
    }

    public void launchNotification(PermissionResultCallback callback) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            String permission = Manifest.permission.POST_NOTIFICATIONS;
            if (checkSelfPermission(permission) == PackageManager.PERMISSION_GRANTED) {
                callback.onResult(Activity.RESULT_OK, null);
            } else if (shouldShowRequestPermissionRationale(permission)) {
                AppUtils.showDialog(this, R.string.capture_service_on_tips_4, new SelectCallback() {
                    @Override
                    public void onEnter() {
                        resultCallback = callback;
                        permissionLauncher.launch(permission);
                    }

                    @Override
                    public void onCancel() {
                        callback.onResult(Activity.RESULT_CANCELED, null);
                    }
                });
            } else {
                resultCallback = callback;
                permissionLauncher.launch(permission);
            }
        } else {
            callback.onResult(Activity.RESULT_OK, null);
        }
    }

    public void launcherContent(PermissionResultCallback callback) {
        resultCallback = callback;
        contentLauncher.launch("text/plain");
    }

    public void showPlayFloatView(String pkgName) {
        binding.getRoot().post(() -> {
            PlayFloatView view = (PlayFloatView) EasyFloat.getView(PlayFloatView.class.getCanonicalName());
            if (view == null) {
                new PlayFloatView(this, pkgName).show();
            } else {
                view.setPkgName(pkgName);
                view.setNeedRemove(false);
            }
        });
        LogUtils.log(LogLevel.LOW, getString(R.string.log_run_manual));
    }

    public void dismissPlayFloatView() {
        PlayFloatView view = (PlayFloatView) EasyFloat.getView(PlayFloatView.class.getCanonicalName());
        if (view != null) view.setNeedRemove(true);
    }
}