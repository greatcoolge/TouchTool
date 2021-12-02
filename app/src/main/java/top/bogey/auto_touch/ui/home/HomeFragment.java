package top.bogey.auto_touch.ui.home;

import android.accessibilityservice.AccessibilityService;
import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import android.provider.Settings;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FragmentHomeBinding;
import top.bogey.auto_touch.ui.MainViewModel;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class HomeFragment extends Fragment {
    private FragmentHomeBinding binding;
    private MainViewModel viewModel;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        binding = FragmentHomeBinding.inflate(inflater, container, false);
        viewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);

        binding.serviceToggle.setOnClickListener(v -> {
            if (AppUtil.isAccessibilityServiceOn(requireActivity())){
                boolean enable = viewModel.isServiceEnable();
                viewModel.saveServiceEnable(!enable);
                if (MainApplication.getService() != null){
                    MainApplication.getService().enable = true;
                }
            } else {
                viewModel.saveServiceEnable(false);
                AppUtil.showSimpleDialog(requireActivity(), R.string.service_open_tips, new SelectCallback() {
                    @Override
                    public void onEnter() {
                        Intent intent = new Intent(Settings.ACTION_ACCESSIBILITY_SETTINGS);
                        requireActivity().startActivity(intent);
                    }

                    @Override
                    public void onCancel() { }
                });
            }
        });
        viewModel.serviceEnable.observe(getViewLifecycleOwner(), aBoolean -> binding.serviceToggle.setChecked(aBoolean));

        binding.captureServiceToggle.setOnClickListener(v -> {
            MainAccessibilityService service = MainApplication.getService();
            if (service != null){
                if (service.binder == null){
                    AppUtil.showSimpleDialog(requireActivity(), R.string.capture_service_open_tips, new SelectCallback() {
                        @Override
                        public void onEnter() {
                            service.startCaptureService(false, () -> binding.captureServiceToggle.setChecked(service.binder != null));
                        }

                        @Override
                        public void onCancel() {
                            binding.captureServiceToggle.setChecked(false);
                        }
                    });
                } else {
                    service.stopCaptureService();
                    binding.captureServiceToggle.setChecked(false);
                }
            } else {
                binding.captureServiceToggle.setChecked(false);
            }
        });
        MainAccessibilityService service = MainApplication.getService();
        binding.captureServiceToggle.setChecked(service != null && service.binder != null);

        binding.tipsButton.setOnClickListener(v -> AppUtil.showSimpleDialog(requireActivity(), R.string.auto_start_tips, new SelectCallback() {
            @Override
            public void onEnter() {
                AppUtil.gotoAutostartSettingIntent(requireActivity());
            }

            @Override
            public void onCancel() { }
        }));

        binding.lockButton.setOnClickListener(v -> AppUtil.showSimpleDialog(requireActivity(), R.string.lock_task_tips, new SelectCallback() {
            @Override
            public void onEnter() {
                if (AppUtil.isAccessibilityServiceOn(requireActivity())){
                    MainAccessibilityService service = MainApplication.getService();
                    if (service != null) {
                        service.performGlobalAction(AccessibilityService.GLOBAL_ACTION_RECENTS);
                    }
                }
            }

            @Override
            public void onCancel() { }
        }));

        return binding.getRoot();
    }
}