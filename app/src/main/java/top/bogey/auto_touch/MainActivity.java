package top.bogey.auto_touch;

import android.content.Context;
import android.content.Intent;
import android.media.projection.MediaProjectionManager;
import android.os.Bundle;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.navigation.ui.AppBarConfiguration;
import androidx.navigation.ui.NavigationUI;

import top.bogey.auto_touch.databinding.ActivityMainBinding;
import top.bogey.auto_touch.util.ResultCallback;

public class MainActivity extends AppCompatActivity {
    static { System.loadLibrary("auto_touch"); }

    private ActivityMainBinding binding;
    private ActivityResultLauncher<Intent> captureLauncher;
    private ResultCallback callback;
    private Intent captureIntent;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        MainApplication.setActivity(this);
        startService(new Intent(this, MainAccessibilityService.class));

        String extra = getIntent().getStringExtra("IsBackground");
        if (extra != null && extra.equals("true")){
            moveTaskToBack(true);
        }

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

    public void launcherCapture(ResultCallback callback){
        if (captureIntent != null){
            callback.onResult(RESULT_OK, captureIntent);
            return;
        }
        this.callback = callback;
        MediaProjectionManager manager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
        captureLauncher.launch(manager.createScreenCaptureIntent());
    }

    public void showPlayView(String pkgName){
//        binding.getRoot().post(() -> {
//            if (playView != null) playView.dismiss();
//            playView = new ConfigPlayView(this, pkgName, () -> playView = null);
//            playView.show();
//        });
    }

    public void dismissPlayView() {
//        binding.getRoot().post(() -> {
//            if (playView != null) playView.dismiss();
//
    }
}

