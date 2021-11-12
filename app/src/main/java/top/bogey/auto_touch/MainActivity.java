package top.bogey.auto_touch;

import androidx.appcompat.app.AppCompatActivity;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.navigation.ui.AppBarConfiguration;
import androidx.navigation.ui.NavigationUI;

import android.os.Bundle;

import top.bogey.auto_touch.databinding.ActivityMainBinding;

public class MainActivity extends AppCompatActivity {
    static { System.loadLibrary("auto_touch"); }

    private ActivityMainBinding binding;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        MainApplication.setActivity(this);
    }

    @Override
    protected void onStart() {
        super.onStart();
        AppBarConfiguration configuration = new AppBarConfiguration.Builder(R.id.homeFragment, R.id.appsFragment, R.id.settingFragment).build();
        NavController controller = Navigation.findNavController(this, R.id.con_view);
        NavigationUI.setupActionBarWithNavController(this, controller, configuration);
        NavigationUI.setupWithNavController(binding.menuMain, controller);
    }
}