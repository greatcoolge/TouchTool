package top.bogey.touch_tool.ui.setting;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.material.bottomsheet.BottomSheetDialogFragment;

import java.util.List;

import top.bogey.touch_tool.databinding.SheetDebugBinding;
import top.bogey.touch_tool.utils.LogUtils;

public class DebugInfoView extends BottomSheetDialogFragment {

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        SheetDebugBinding binding = SheetDebugBinding.inflate(inflater);
        List<DebugInfo> logs = LogUtils.getLogs(requireContext(), null);
        StringBuilder buffer = new StringBuilder();
        for (DebugInfo log : logs) {
            buffer.append(log.getDebugInfo()).append('\n');
        }
        binding.debugInfo.setText(buffer.toString());
        return binding.getRoot();
    }
}
