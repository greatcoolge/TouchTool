package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import com.tencent.mmkv.MMKV;

import java.util.HashMap;
import java.util.Map;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.databinding.FloatOverseeBinding;
import top.bogey.touch_tool.room.bean.Task;
import top.bogey.touch_tool.room.bean.TaskStatus;
import top.bogey.touch_tool.room.data.TaskCallable;
import top.bogey.touch_tool.utils.RunStateCallback;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;
import top.bogey.touch_tool.utils.easy_float.SidePattern;

@SuppressLint("ViewConstructor")
public class OverSeeFloatView extends FrameLayout implements FloatViewInterface, RunStateCallback {
    public static final String OVERSEE_MODE = "oversee_mode";

    private final FloatOverseeBinding binding;
    private OverseeMode overseeMode;

    private final Map<TaskCallable, OverSeeFloatViewItem> itemMap = new HashMap<>();

    public OverSeeFloatView(@NonNull Context context, OverseeMode mode) {
        super(context);
        binding = FloatOverseeBinding.inflate(LayoutInflater.from(context), this, true);
        if (mode == null) {
            String string = MMKV.defaultMMKV().decodeString(OVERSEE_MODE);
            if (string != null) {
                mode = OverseeMode.values()[Integer.parseInt(string)];
            } else mode = OverseeMode.ALL;
        }
        setOverseeMode(mode);
    }

    @Override
    public void show() {
        dismiss();
        EasyFloat.with(getContext())
                .setLayout(this)
                .setSidePattern(SidePattern.HORIZONTAL)
                .setGravity(FloatGravity.LEFT_CENTER, 0, 0)
                .setBorder(20, 20, 0, 0)
                .setTag(OverSeeFloatView.class.getCanonicalName())
                .setAlwaysShow(true)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(OverSeeFloatView.class.getCanonicalName());
    }

    public void setOverseeMode(OverseeMode overseeMode) {
        this.overseeMode = overseeMode;
        itemMap.clear();
        binding.buttonBox.removeAllViews();
        MainAccessibilityService service = MainApplication.getService();
        if (service != null) {
            for (TaskCallable runningTask : service.getRunningTasks(this)) {
                addNewTask(runningTask);
            }
        }
    }

    @Override
    public void onNewTask(TaskCallable callable) {
        post(() -> addNewTask(callable));
    }

    public void addNewTask(TaskCallable callable) {
        Task task = callable.getTask();
        if (overseeMode == OverseeMode.ACROSS_APP) {
            if (!task.isAcrossApp() && task.getStatus() != TaskStatus.TIME) return;
        }
        if (overseeMode == OverseeMode.NOT_MANUAL) {
            if (task.getStatus() == TaskStatus.MANUAL) return;
        }
        OverSeeFloatViewItem overSeeFloatViewItem = new OverSeeFloatViewItem(getContext(), callable);
        binding.buttonBox.addView(overSeeFloatViewItem);
        itemMap.put(callable, overSeeFloatViewItem);
        binding.oversee.setVisibility(VISIBLE);
    }

    @Override
    public void onTaskProgress(TaskCallable callable, int progress) {
        OverSeeFloatViewItem overSeeFloatViewItem = itemMap.get(callable);
        if (overSeeFloatViewItem != null) overSeeFloatViewItem.refreshProgress(progress);
    }

    @Override
    public void onTaskEnd(TaskCallable callable) {
        post(() -> {
            OverSeeFloatViewItem overSeeFloatViewItem = itemMap.remove(callable);
            if (overSeeFloatViewItem != null) binding.buttonBox.removeView(overSeeFloatViewItem);
            binding.oversee.setVisibility(itemMap.size() > 0 ? VISIBLE : INVISIBLE);
        });
    }
}
