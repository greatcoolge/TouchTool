package top.bogey.touch_tool.room.data;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.ui.setting.LogLevel;
import top.bogey.touch_tool.ui.setting.RunningUtils;

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
            service.runTask(TaskRepository.getInstance(service).getTaskById(inputData.getString("id")), null);
            RunningUtils.log(LogLevel.HIGH, service.getString(R.string.log_do_job, inputData.getString("title")));
        }
        return Result.success();
    }
}
