package top.bogey.auto_touch.ui.setting;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;

import top.bogey.auto_touch.databinding.FragmentSettingBinding;
import top.bogey.auto_touch.util.AppUtil;

public class SettingFragment extends Fragment {
    private FragmentSettingBinding binding;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        binding = FragmentSettingBinding.inflate(inflater, container, false);

        StringBuilder builder = new StringBuilder();
        for (String string : AppUtil.strings) {
            builder.append(string).append("\n");
        }
        binding.logText.setText(builder.toString());

        return binding.getRoot();
    }
}