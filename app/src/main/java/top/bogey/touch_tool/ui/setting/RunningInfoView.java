package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.Fragment;

import com.amrdeveloper.treeview.TreeNodeManager;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.ViewSettingRunningBinding;

public class RunningInfoView extends Fragment {

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        ViewSettingRunningBinding binding = ViewSettingRunningBinding.inflate(inflater);
        TreeNodeManager manager = new TreeNodeManager();
        RunningInfoTreeAdapter adapter = new RunningInfoTreeAdapter(manager);
        binding.getRoot().setAdapter(adapter);

        return binding.getRoot();
    }
}
