package top.bogey.touch_tool.database.data;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.LogUtils;

public class TaskWorker extends Worker {
    public TaskWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {
        MainAccessibilityService service = MainApplication.getService();
        if (service != null) {
            Data inputData = getInputData();
            Task task = TaskRepository.getInstance().getTaskById(inputData.getString("id"));
            service.runTask(task, null);
            LogUtils.log(LogLevel.HIGH, service.getString(R.string.log_run_time_task, task.getTitle()));
        }
        return Result.success();
    }
}
