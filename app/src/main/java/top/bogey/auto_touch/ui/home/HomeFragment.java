package top.bogey.auto_touch.ui.home;

import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import android.provider.Settings;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

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

        return binding.getRoot();
    }
}