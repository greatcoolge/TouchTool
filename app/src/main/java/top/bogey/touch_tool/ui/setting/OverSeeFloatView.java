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
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskType;
import top.bogey.touch_tool.database.data.TaskRunnable;
import top.bogey.touch_tool.databinding.FloatOverseeBinding;
import top.bogey.touch_tool.utils.TaskRunningCallback;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;
import top.bogey.touch_tool.utils.easy_float.SidePattern;

@SuppressLint("ViewConstructor")
public class OverSeeFloatView extends FrameLayout implements FloatViewInterface, TaskRunningCallback {
    public static final String OVERSEE_MODE = "oversee_mode";

    private final FloatOverseeBinding binding;
    private OverseeMode overseeMode;

    private final Map<TaskRunnable, OverSeeFloatViewItem> itemMap = new HashMap<>();

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
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        MainAccessibilityService service = MainApplication.getService();
        if (service != null) {
            service.removeRunStateCallback(this);
        }
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
            for (TaskRunnable runningTask : service.getRunningTasks(this)) {
                addNewTask(runningTask);
            }
        }
    }

    @Override
    public void onStart(TaskRunnable runnable) {
        post(() -> addNewTask(runnable));
    }

    public void addNewTask(TaskRunnable runnable) {
        Task task = runnable.getTask();
        if (overseeMode == OverseeMode.ACROSS_APP) {
            if (!task.isAcrossAppTask()) return;
        }
        if (overseeMode == OverseeMode.NOT_MANUAL) {
            if (task.getType() == TaskType.MANUAL) return;
        }
        OverSeeFloatViewItem overSeeFloatViewItem = new OverSeeFloatViewItem(getContext(), runnable);
        binding.buttonBox.addView(overSeeFloatViewItem);
        itemMap.put(runnable, overSeeFloatViewItem);
        binding.oversee.setVisibility(VISIBLE);
    }

    @Override
    public void onProgress(TaskRunnable runnable, int progress) {
        OverSeeFloatViewItem overSeeFloatViewItem = itemMap.get(runnable);
        if (overSeeFloatViewItem != null) overSeeFloatViewItem.refreshProgress(progress);
    }

    @Override
    public void onEnd(TaskRunnable runnable, boolean success) {
        post(() -> {
            OverSeeFloatViewItem overSeeFloatViewItem = itemMap.remove(runnable);
            if (overSeeFloatViewItem != null) binding.buttonBox.removeView(overSeeFloatViewItem);
            binding.oversee.setVisibility(itemMap.size() > 0 ? VISIBLE : INVISIBLE);
        });
    }
}
