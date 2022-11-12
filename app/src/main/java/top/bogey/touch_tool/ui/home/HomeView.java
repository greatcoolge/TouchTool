package top.bogey.touch_tool.ui.home;

import android.accessibilityservice.AccessibilityService;
import android.app.Activity;
import android.content.Intent;
import android.content.res.ColorStateList;
import android.net.Uri;
import android.os.Bundle;
import android.provider.Settings;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainActivity;
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
            if (service != null && service.isServiceConnected()) {
                service.setServiceEnabled(!service.isServiceEnabled());
            } else {
                AppUtils.showDialog(requireContext(), R.string.accessibility_service_on_tips, new SelectCallback() {
                    @Override
                    public void onEnter() {
                        Intent intent = new Intent(Settings.ACTION_ACCESSIBILITY_SETTINGS);
                        requireActivity().startActivity(intent);
                    }
                });
            }
        });
        MainAccessibilityService.serviceEnabled.observe(getViewLifecycleOwner(), this::setServiceChecked);

        binding.captureServiceButton.setOnClickListener(view -> {
            MainActivity activity = MainApplication.getActivity();
            if (activity != null) activity.launchNotification((code, intent) -> {
                if (code == Activity.RESULT_OK) {
                    MainAccessibilityService service = MainApplication.getService();
                    if (service != null && service.isServiceConnected()) {
                        boolean enabled = Boolean.TRUE.equals(MainAccessibilityService.captureEnabled.getValue());
                        if (enabled) {
                            service.stopCaptureService();
                        } else {
                            Toast.makeText(requireActivity(), R.string.capture_service_on_tips_1, Toast.LENGTH_LONG).show();
                            service.startCaptureService(false, null);
                        }
                    } else {
                        Toast.makeText(requireActivity(), R.string.capture_service_on_tips_3, Toast.LENGTH_LONG).show();
                    }
                }
            });
        });
        MainAccessibilityService.captureEnabled.observe(getViewLifecycleOwner(), this::setCaptureChecked);

        binding.openBackgroundPopButton.setOnClickListener(v -> AppUtils.gotoAppDetailSetting(requireActivity()));
        binding.autoRunButton.setOnClickListener(v -> AppUtils.gotoAppDetailSetting(requireActivity()));

        binding.lockTaskButton.setOnClickListener(v -> {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null && service.isServiceConnected()) {
                service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_RECENTS);
            }
        });

        binding.tutorialButton.setOnClickListener(v -> {
            try {
                Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse("https://docs.qq.com/doc/p/0f4de9e03534db3780876b90965e9373e4af93f0"));
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                startActivity(intent);
            } catch (Exception ignored) {
            }
        });

        return binding.getRoot();
    }

    private void setServiceChecked(boolean checked) {
        if (checked) {
            binding.accessibilityServiceButton.setCardBackgroundColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorPrimary, 0));
            binding.accessibilityServiceIcon.setImageTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorOnPrimary, 0)));
            binding.accessibilityServiceTitle.setTextColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorOnPrimary, 0));
            binding.accessibilityServiceTitle.setText(R.string.accessibility_service_on);
            binding.accessibilityServiceSubtitle.setTextColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorOnPrimary, 0));
        } else {
            binding.accessibilityServiceButton.setCardBackgroundColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorSurfaceVariant, 0));
            binding.accessibilityServiceIcon.setImageTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorPrimary, 0)));
            binding.accessibilityServiceTitle.setTextColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorPrimary, 0));
            binding.accessibilityServiceTitle.setText(R.string.accessibility_service_off);
            binding.accessibilityServiceSubtitle.setTextColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorPrimary, 0));
        }
        MainAccessibilityService service = MainApplication.getService();
        if (service != null && service.isServiceConnected()) {
            binding.accessibilityServiceSubtitle.setText(R.string.accessibility_service_on_2);
        } else {
            binding.accessibilityServiceSubtitle.setText(R.string.accessibility_service_off_2);
        }
        binding.lockTaskButton.setVisibility(checked ? View.VISIBLE : View.GONE);
    }

    private void setCaptureChecked(boolean checked) {
        if (checked) {
            binding.captureServiceButton.setCardBackgroundColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorPrimary, 0));
            binding.captureServiceIcon.setImageTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorOnPrimary, 0)));
            binding.captureServiceTitle.setTextColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorOnPrimary, 0));
            binding.captureServiceTitle.setText(R.string.capture_service_on);
        } else {
            binding.captureServiceButton.setCardBackgroundColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorSurfaceVariant, 0));
            binding.captureServiceIcon.setImageTintList(ColorStateList.valueOf(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorPrimary, 0)));
            binding.captureServiceTitle.setTextColor(DisplayUtils.getAttrColor(requireContext(), com.google.android.material.R.attr.colorPrimary, 0));
            binding.captureServiceTitle.setText(R.string.capture_service_off);
        }
    }
}
