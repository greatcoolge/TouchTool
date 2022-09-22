package top.bogey.touch_tool.ui.setting;

import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.preference.DropDownPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.PreferenceManager;
import androidx.preference.SwitchPreferenceCompat;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.utils.LogUtils;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

public class SettingView extends PreferenceFragmentCompat {

    @Override
    public void onCreatePreferences(@Nullable Bundle savedInstanceState, @Nullable String rootKey) {
        PreferenceManager preferenceManager = getPreferenceManager();
        preferenceManager.setSharedPreferencesName(MainAccessibilityService.SAVE_PATH);
        setPreferencesFromResource(R.xml.setting, null);

        SwitchPreferenceCompat showDebugDialog = findPreference("show_debug_dialog");
        Preference showDebug = findPreference("show_debug");
        SwitchPreferenceCompat actionDebug = findPreference(LogUtils.ACTION_DEBUG);
        if (actionDebug != null){
            actionDebug.setOnPreferenceChangeListener((preference, newValue) -> {
                if (Boolean.FALSE.equals(newValue)) LogUtils.closeLog(requireContext());
                if (showDebugDialog != null) showDebugDialog.setVisible(Boolean.TRUE.equals(newValue));
                if (showDebug != null) showDebug.setVisible(Boolean.TRUE.equals(newValue));
                return true;
            });
            if (showDebugDialog != null) showDebugDialog.setVisible(actionDebug.isChecked());
            if (showDebug != null) showDebug.setVisible(actionDebug.isChecked());
        }

        if (showDebug != null){
            showDebug.setOnPreferenceClickListener(preference -> {
                MainActivity activity = MainApplication.getActivity();
                if (activity != null){
                    NavController controller = Navigation.findNavController(activity, R.id.con_view);
                    controller.navigate(SettingViewDirections.actionSettingToDebugInfo());
                }
                return true;
            });
        }

        if (showDebugDialog != null){
            if (actionDebug != null){
                showDebugDialog.setOnPreferenceChangeListener((preference, newValue) -> {
                EasyFloat.dismiss(DebugFloatView.class.getCanonicalName());
                if (Boolean.TRUE.equals(newValue) && actionDebug.isChecked()){
                    new DebugFloatView(requireContext()).show();
                }
                return true;
            });
                showDebugDialog.setChecked(EasyFloat.getView(DebugFloatView.class.getCanonicalName()) != null && actionDebug.isChecked());
            } else {
                showDebugDialog.setVisible(false);
            }
        }

        DropDownPreference nightMode = findPreference(MainApplication.NIGHT_MODE);
        if (nightMode != null){
            nightMode.setOnPreferenceChangeListener((preference, newValue) -> {
                int nightModeValue = Integer.parseInt(String.valueOf(newValue));
                MainApplication.initNightMode(nightModeValue);
                nightMode.setSummary(nightMode.getEntry());
                MainApplication.getActivity().recreate();
                return true;
            });
            nightMode.setSummary(nightMode.getEntry());
        }

        Preference version = findPreference("version");
        if (version != null){
            PackageManager manager = requireContext().getPackageManager();
            try {
                PackageInfo packageInfo = manager.getPackageInfo(requireContext().getPackageName(), 0);
                version.setSummary(packageInfo.versionName + "(" + packageInfo.versionCode + ")");
            } catch (PackageManager.NameNotFoundException e) {
                e.printStackTrace();
            }
            version.setOnPreferenceClickListener(preference -> {
                try {
                    String address = "market://details?id=" + requireContext().getPackageName();
                    Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(address));
                    intent.setPackage("com.coolapk.market");
                    intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    startActivity(intent);
                } catch (Exception ignored){}
                return true;
            });
        }

        Preference sourceCode = findPreference("source_code");
        if (sourceCode != null){
            sourceCode.setOnPreferenceClickListener(preference -> {
                try {
                    Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse("https://github.com/mr-bogey/AutoTouch"));
                    intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    startActivity(intent);
                } catch (Exception ignored){ }
                return true;
            });
        }
    }

}
