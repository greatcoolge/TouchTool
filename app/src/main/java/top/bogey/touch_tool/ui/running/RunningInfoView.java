package top.bogey.touch_tool.ui.running;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.amrdeveloper.treeview.TreeNodeManager;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;

import top.bogey.touch_tool.databinding.ViewSettingRunningBinding;

public class RunningInfoView extends BottomSheetDialogFragment {

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        ViewSettingRunningBinding binding = ViewSettingRunningBinding.inflate(inflater);
        RunningInfoTreeAdapter adapter = new RunningInfoTreeAdapter(new TreeNodeManager());
        binding.taskBox.setAdapter(adapter);

        return binding.getRoot();
    }
}
