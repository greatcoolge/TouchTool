package top.bogey.touch_tool.ui.setting;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import top.bogey.touch_tool.databinding.ViewDebugBinding;

public class DebugInfoView extends Fragment {

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        ViewDebugBinding binding = ViewDebugBinding.inflate(inflater);

        DebugInfoRecyclerViewAdapter adapter = new DebugInfoRecyclerViewAdapter(this);
        binding.getRoot().setAdapter(adapter);

        return binding.getRoot();
    }
}
