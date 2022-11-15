package top.bogey.touch_tool.ui.play;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.bean.TaskType;
import top.bogey.touch_tool.database.data.TaskRepository;
import top.bogey.touch_tool.database.data.TaskRunnable;
import top.bogey.touch_tool.databinding.FloatPlayBinding;
import top.bogey.touch_tool.ui.setting.SettingSave;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.TaskRunningCallback;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;
import top.bogey.touch_tool.utils.easy_float.SidePattern;

@SuppressLint("ViewConstructor")
public class PlayFloatView extends FrameLayout implements FloatViewInterface, TaskRunningCallback {
    private final FloatPlayBinding binding;

    private boolean clickFirst = false;
    private OverseeMode overseeMode;

    private boolean needRemove = true;

    public PlayFloatView(@NonNull Context context, String pkgName) {
        super(context);

        binding = FloatPlayBinding.inflate(LayoutInflater.from(context), this, true);

        setOverseeMode(SettingSave.getInstance().getRunningOverseeMode());

        binding.closeButton.setOnClickListener(v -> {
            if (clickFirst && overseeMode == OverseeMode.CLOSED) dismiss();
            else {
                clickFirst = true;
                postDelayed(() -> clickFirst = false, 500);
                ViewGroup.LayoutParams params = binding.closeButton.getLayoutParams();
                if (binding.buttonBox.getVisibility() == VISIBLE) {
                    binding.buttonBox.setVisibility(GONE);
                    binding.closeButton.setIconResource(R.drawable.icon_down);

                    params.height = DisplayUtils.dp2px(context, 40);
                } else {
                    binding.buttonBox.setVisibility(VISIBLE);
                    binding.closeButton.setIconResource(R.drawable.icon_up);

                    params.height = DisplayUtils.dp2px(context, 30);
                }
                binding.closeButton.setLayoutParams(params);
            }
        });

        setPkgName(pkgName);
    }

    @Override
    public void show() {
        dismiss();
        EasyFloat.with(getContext()).setLayout(this).setSidePattern(SidePattern.HORIZONTAL).setGravity(FloatGravity.RIGHT_CENTER, 0, 0).setBorder(20, 20, 0, 0).setTag(PlayFloatView.class.getCanonicalName()).setAlwaysShow(true).show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(PlayFloatView.class.getCanonicalName());
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        MainAccessibilityService service = MainApplication.getService();
        if (service != null && service.isServiceEnabled()) {
            service.removeRunStateCallback(this);
        }
    }

    public void setNeedRemove(boolean needRemove) {
        this.needRemove = needRemove;
        post(() -> {
            for (int i = binding.buttonBox.getChildCount() - 1; i >= 0; i--) {
                PlayFloatViewItem view = (PlayFloatViewItem) binding.buttonBox.getChildAt(i);
                if (view.isFree() && needRemove) {
                    binding.buttonBox.removeView(view);
                }
                view.setNeedRemove(needRemove);
            }
            checkShow();
        });
    }

    public void setPkgName(String pkgName) {
        List<Task> tasks = getManualTasks(pkgName);
        for (int i = binding.buttonBox.getChildCount() - 1; i >= 0; i--) {
            PlayFloatViewItem view = (PlayFloatViewItem) binding.buttonBox.getChildAt(i);
            if (view.isFree()) {
                binding.buttonBox.removeView(view);
            } else {
                boolean flag = true;
                for (int j = tasks.size() - 1; j >= 0; j--) {
                    Task task = tasks.get(j);
                    // 新的手动任务已经存在于UI上了，跳过
                    if (task.getId().equals(view.getTask().getId())) {
                        tasks.remove(j);
                        flag = false;
                        break;
                    }
                }
                if (flag) {
                    view.setNeedRemove(true);
                }
            }
        }

        for (Task task : tasks) {
            binding.buttonBox.addView(new PlayFloatViewItem(getContext(), task));
        }
    }

    private List<Task> getManualTasks(String pkgName) {
        List<Task> comTasks = TaskRepository.getInstance().getTasksByPkgName(getContext().getString(R.string.common_package_name));
        if (pkgName != null && !"null".equals(pkgName)) {
            List<Task> pkgTasks = TaskRepository.getInstance().getTasksByPkgName(pkgName);
            if (comTasks.isEmpty()) comTasks.addAll(pkgTasks);
            else {
                for (Task pkgTask : pkgTasks) {
                    for (Task comTask : comTasks) {
                        if (!pkgTask.getId().equals(comTask.getId())) {
                            comTasks.add(pkgTask);
                            break;
                        }
                    }
                }
            }
        }

        for (int i = comTasks.size() - 1; i >= 0; i--) {
            Task task = comTasks.get(i);
            if (task.getType() != TaskType.MANUAL) comTasks.remove(i);
        }

        return comTasks;
    }

    public void setOverseeMode(OverseeMode overseeMode) {
        this.overseeMode = overseeMode;

        MainAccessibilityService service = MainApplication.getService();
        if (service != null && service.isServiceEnabled()) {
            List<TaskRunnable> runnableList = service.getRunningTasks(this);
            int childCount = binding.buttonBox.getChildCount();
            for (TaskRunnable runnable : runnableList) {
                boolean flag = true;
                for (int i = 0; i < childCount; i++) {
                    PlayFloatViewItem view = (PlayFloatViewItem) binding.buttonBox.getChildAt(i);
                    // 如果任务正在被监听，就不管他
                    if (view.isOversee() && view.getTask().getId().equals(runnable.getTask().getId())) {
                        flag = false;
                        break;
                    }
                }
                if (flag) {
                    onStart(runnable);
                }
            }
        }

        checkShow();
    }

    public void checkShow() {
        postDelayed(() -> {
            if (binding.buttonBox.getChildCount() == 0 && needRemove && overseeMode == OverseeMode.CLOSED) dismiss();
        }, 100);
    }

    @Override
    public void onStart(TaskRunnable runnable) {
        Task task = runnable.getTask();
        if (overseeMode == OverseeMode.CLOSED) return;

        if (overseeMode == OverseeMode.ACROSS_APP) {
            if (!task.isAcrossAppTask()) return;
        }
        if (overseeMode == OverseeMode.NOT_MANUAL) {
            if (task.getType() == TaskType.MANUAL) return;
        }
        post(() -> binding.buttonBox.addView(new PlayFloatViewItem(getContext(), runnable)));
    }

    @Override
    public void onEnd(TaskRunnable runnable, boolean succeed) {
        checkShow();
    }

    @Override
    public void onProgress(TaskRunnable runnable, int percent) {

    }
}
