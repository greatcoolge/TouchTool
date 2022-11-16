package top.bogey.touch_tool.ui.setting;

import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewSettingBinding;
import top.bogey.touch_tool.ui.play.OverseeMode;
import top.bogey.touch_tool.ui.running.RunningInfoView;

public class SettingView extends Fragment {

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        ViewSettingBinding binding = ViewSettingBinding.inflate(inflater, container, false);

        binding.keepAliveSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> SettingSave.getInstance().setKeepAlive(requireContext(), isChecked));
        binding.keepAliveSwitch.setChecked(SettingSave.getInstance().isKeepAlive());

        binding.actionTouchOffsetSlider.addOnChangeListener((slider, value, fromUser) -> {
            if (fromUser) SettingSave.getInstance().setActionTouchOffset((int) value);
        });
        binding.actionTouchOffsetSlider.setLabelFormatter(value -> requireContext().getString(R.string.setting_action_touch_offset_format, (int) value));
        binding.actionTouchOffsetSlider.setValue(SettingSave.getInstance().getActionTouchOffset());

        binding.actionRecordDelaySlider.addOnChangeListener((slider, value, fromUser) -> {
            if (fromUser) SettingSave.getInstance().setActionRecordDelay((int) value);
        });
        binding.actionRecordDelaySlider.setLabelFormatter(value -> requireContext().getString(R.string.setting_action_record_delay_format, (int) value));
        binding.actionRecordDelaySlider.setValue(SettingSave.getInstance().getActionRecordDelay());

        binding.eventTimeoutSlider.addOnChangeListener((slider, value, fromUser) -> {
            if (fromUser) SettingSave.getInstance().setEventTimeout((int) value);
        });
        binding.eventTimeoutSlider.setLabelFormatter(value -> requireContext().getString(R.string.setting_event_timeout_format, (int) value));
        binding.eventTimeoutSlider.setValue(SettingSave.getInstance().getEventTimeout());

        binding.overseeModeGroup.addOnButtonCheckedListener((group, checkedId, isChecked) -> {
            if (isChecked) {
                View view = group.findViewById(checkedId);
                OverseeMode overseeMode = OverseeMode.values()[group.indexOfChild(view)];
                SettingSave.getInstance().setRunningOverseeMode(overseeMode);
            }
        });
        binding.overseeModeGroup.check(binding.overseeModeGroup.getChildAt(SettingSave.getInstance().getRunningOverseeMode().ordinal()).getId());

        binding.logSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            SettingSave.getInstance().setRunningLog(isChecked);
            if (!isChecked) LogUtils.closeLog();
        });
        binding.logSwitch.setChecked(SettingSave.getInstance().isRunningLog());

        binding.logDialogButton.setOnClickListener(v -> new LogFloatView(requireContext()).show());

        binding.runningTaskInfoSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            SettingSave.getInstance().setRunningTaskInfo(isChecked);
            if (!isChecked) LogUtils.cleanRunningInfo();
        });
        binding.runningTaskInfoSwitch.setChecked(SettingSave.getInstance().isRunningTaskInfo());

        binding.runningTaskInfoButton.setOnClickListener(v -> {
            RunningInfoView infoView = new RunningInfoView();
            infoView.show(requireActivity().getSupportFragmentManager(), null);
        });

        binding.nightModeGroup.addOnButtonCheckedListener((group, checkedId, isChecked) -> {
            if (isChecked) {
                View view = group.findViewById(checkedId);
                NightMode nightMode = NightMode.values()[group.indexOfChild(view)];
                SettingSave.getInstance().setNightMode(nightMode);
            }
        });
        binding.nightModeGroup.check(binding.nightModeGroup.getChildAt(SettingSave.getInstance().getNightMode().ordinal()).getId());

        binding.dynamicColorSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> SettingSave.getInstance().setDynamicColor(requireContext(), isChecked));
        binding.dynamicColorSwitch.setChecked(SettingSave.getInstance().isDynamicColor());

        binding.versionButton.setOnClickListener(v -> {
            try {
                String address = "market://details?id=" + requireContext().getPackageName();
                Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(address));
                intent.setPackage("com.coolapk.market");
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                startActivity(intent);
            } catch (Exception ignored) {
            }
        });

        PackageManager manager = requireContext().getPackageManager();
        try {
            PackageInfo packageInfo = manager.getPackageInfo(requireContext().getPackageName(), 0);
            binding.versionText.setText(requireContext().getString(R.string.setting_version_format, packageInfo.versionName, packageInfo.versionCode));
        } catch (PackageManager.NameNotFoundException e) {
            e.printStackTrace();
        }

        binding.sourceCodeButton.setOnClickListener(v -> {
            try {
                Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse("https://github.com/mr-bogey/TouchTool"));
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                startActivity(intent);
            } catch (Exception ignored) {
            }
        });

        return binding.getRoot();
    }
}
