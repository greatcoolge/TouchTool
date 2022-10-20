package top.bogey.touch_tool.ui.setting;

import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.inputmethod.EditorInfo;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.navigation.NavController;
import androidx.navigation.Navigation;
import androidx.preference.DropDownPreference;
import androidx.preference.EditTextPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.PreferenceManager;
import androidx.preference.SwitchPreferenceCompat;

import com.tencent.mmkv.MMKV;

import java.lang.reflect.Field;

import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.ui.record.RecordFloatView;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

public class SettingView extends PreferenceFragmentCompat {

    @Override
    public void onCreatePreferences(@Nullable Bundle savedInstanceState, @Nullable String rootKey) {
        PreferenceManager preferenceManager = getPreferenceManager();
        preferenceManager.setSharedPreferencesName("setting");
        try {
            Field preferences = preferenceManager.getClass().getDeclaredField("mSharedPreferences");
            preferences.setAccessible(true);
            preferences.set(preferenceManager, MMKV.defaultMMKV());
        } catch (NoSuchFieldException | IllegalAccessException e) {
            e.printStackTrace();
        }

        setPreferencesFromResource(R.xml.setting, rootKey);

        SwitchPreferenceCompat keepAlive = findPreference("keep_alive");
        if (keepAlive != null){
            KeepAliveService aliveService = MainApplication.getAliveService();
            keepAlive.setChecked(aliveService != null);
            keepAlive.setOnPreferenceChangeListener((preference, newValue) -> {
                requireContext().stopService(new Intent(requireContext(), KeepAliveService.class));
                if (Boolean.TRUE.equals(newValue)){
                    requireContext().startService(new Intent(requireContext(), KeepAliveService.class));
                }
                return true;
            });
        }

        EditTextPreference recordDelay = findPreference(RecordFloatView.ACTION_RECORD_DELAY);
        if (recordDelay != null){
            recordDelay.setOnBindEditTextListener(editText -> {
                editText.setInputType(EditorInfo.TYPE_CLASS_NUMBER);
                editText.addTextChangedListener(new TextWatcher() {
                    @Override
                    public void beforeTextChanged(CharSequence s, int start, int count, int after) {

                    }

                    @Override
                    public void onTextChanged(CharSequence s, int start, int before, int count) {

                    }

                    @Override
                    public void afterTextChanged(Editable s) {
                        if (s != null && !s.toString().isEmpty()){
                            int delay = Integer.parseInt(s.toString());
                            if (delay > 1000) {
                                editText.setText(String.valueOf(1000));
                            }
                        }
                    }
                });
            });

            recordDelay.setOnPreferenceChangeListener((preference, newValue) -> {
                recordDelay.setSummary(requireContext().getString(R.string.action_record_delay_curr_value, newValue));
                return true;
            });

            String value = MMKV.defaultMMKV().decodeString(RecordFloatView.ACTION_RECORD_DELAY);
            recordDelay.setSummary(requireContext().getString(R.string.action_record_delay_curr_value, value));
        }

        SwitchPreferenceCompat runningLogDialog = findPreference("running_log_dialog");
        SwitchPreferenceCompat runningLog = findPreference(RunningUtils.RUNNING_LOG);
        if (runningLog != null){
            runningLog.setOnPreferenceChangeListener((preference, newValue) -> {
                if (Boolean.FALSE.equals(newValue)) RunningUtils.closeLog();
                if (runningLogDialog != null) {
                    runningLogDialog.setVisible(Boolean.TRUE.equals(newValue));
                    runningLogDialog.setChecked(EasyFloat.getView(LogFloatView.class.getCanonicalName()) != null && Boolean.TRUE.equals(newValue));
                }
                return true;
            });
            if (runningLogDialog != null) runningLogDialog.setVisible(runningLog.isChecked());
        }

        if (runningLogDialog != null){
            if (runningLog != null){
                runningLogDialog.setOnPreferenceChangeListener((preference, newValue) -> {
                    EasyFloat.dismiss(LogFloatView.class.getCanonicalName());
                    if (Boolean.TRUE.equals(newValue) && runningLog.isChecked()){
                        new LogFloatView(requireContext()).show();
                    }
                    return true;
                });
                runningLogDialog.setChecked(EasyFloat.getView(LogFloatView.class.getCanonicalName()) != null && runningLog.isChecked());
            }
        }

        Preference runningTaskInfo = findPreference("running_task_info");
        if (runningTaskInfo != null){
            runningTaskInfo.setOnPreferenceClickListener(preference -> {
                MainActivity activity = MainApplication.getActivity();
                if (activity != null){
                    NavController controller = Navigation.findNavController(activity, R.id.con_view);
                    controller.navigate(SettingViewDirections.actionSettingToDebugInfo());
                }
                return true;
            });
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
                    Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse("https://github.com/mr-bogey/TouchTool"));
                    intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    startActivity(intent);
                } catch (Exception ignored){ }
                return true;
            });
        }
    }

}
