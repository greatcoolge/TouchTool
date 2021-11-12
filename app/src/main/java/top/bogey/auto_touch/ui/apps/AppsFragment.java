package top.bogey.auto_touch.ui.apps;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;

import top.bogey.auto_touch.databinding.FragmentAppsBinding;

public class AppsFragment extends Fragment {
    private FragmentAppsBinding binding;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        binding = FragmentAppsBinding.inflate(inflater, container, false);

        return binding.getRoot();
    }
}