package top.bogey.touch_tool.ui.home;

import android.accessibilityservice.AccessibilityService;
import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.provider.Settings;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.google.android.material.button.MaterialButton;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewHomeBinding;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.SelectCallback;

public class HomeView extends Fragment {
    private ViewHomeBinding binding;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = ViewHomeBinding.inflate(inflater, container, false);

        binding.accessibilityServiceButton.setOnClickListener(view -> {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null && service.isServiceConnected()){
                boolean enabled = Boolean.TRUE.equals(MainAccessibilityService.serviceEnabled.getValue());
                service.setServiceEnabled(!enabled);
            } else {
                AppUtils.showDialog(requireContext(), R.string.accessibility_service_on_tips, new SelectCallback(){
                    @Override
                    public void onEnter() {
                        Intent intent = new Intent(Settings.ACTION_ACCESSIBILITY_SETTINGS);
                        requireActivity().startActivity(intent);
                    }
                });
            }
        });
        MainAccessibilityService.serviceEnabled.observe(getViewLifecycleOwner(), aBoolean ->
                setChecked(binding.accessibilityServiceButton, aBoolean, R.string.accessibility_service_on, R.string.accessibility_service_off));

        binding.captureServiceButton.setOnClickListener(view -> {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null && service.isServiceConnected() && Boolean.TRUE.equals(MainAccessibilityService.serviceEnabled.getValue())){
                boolean enabled = Boolean.TRUE.equals(MainAccessibilityService.captureEnabled.getValue());
                if (enabled){
                    service.stopCaptureService();
                } else {
                    Toast.makeText(requireActivity(), R.string.capture_service_on_tips_1, Toast.LENGTH_LONG).show();
                    service.startCaptureService(false, null);
                }
            } else {
                Toast.makeText(requireActivity(), R.string.accessibility_service_off, Toast.LENGTH_LONG).show();
            }
        });
        MainAccessibilityService.captureEnabled.observe(getViewLifecycleOwner(), aBoolean ->
                setChecked(binding.captureServiceButton, aBoolean, R.string.capture_service_on, R.string.capture_service_off));

        binding.autoRunButton.setOnClickListener(v -> AppUtils.showDialog(requireContext(), R.string.auto_run_tips, new SelectCallback(){
            @Override
            public void onEnter() {
                AppUtils.gotoAppDetailSetting(requireActivity());
            }
        }));

        binding.lockTaskButton.setOnClickListener(v -> AppUtils.showDialog(requireContext(), R.string.lock_task_tips, new SelectCallback(){
            @Override
            public void onEnter() {
                MainAccessibilityService service = MainApplication.getService();
                if (service != null && service.isServiceConnected()){
                    service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_RECENTS);
                }
            }
        }));

        return binding.getRoot();
    }

    @SuppressLint("PrivateResource")
    private void setChecked(MaterialButton button, boolean checked, int onId, int offId){
        if (checked){
            button.setTextColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorOnPrimary, 0));
            button.setBackgroundColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorPrimary, 0));
            button.setRippleColorResource(com.google.android.material.R.color.m3_button_ripple_color_selector);
            button.setText(onId);
        } else {
            button.setTextColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorOnSecondaryContainer, 0));
            button.setBackgroundColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorSecondaryContainer, 0));
            button.setRippleColorResource(com.google.android.material.R.color.m3_tonal_button_ripple_color_selector);
            button.setText(offId);
        }
    }
}
