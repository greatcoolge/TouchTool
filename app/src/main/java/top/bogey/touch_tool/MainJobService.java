package top.bogey.touch_tool;

import android.app.job.JobParameters;
import android.app.job.JobService;
import android.os.PersistableBundle;
import android.util.Log;

import top.bogey.touch_tool.room.data.TaskRepository;

public class MainJobService extends JobService {

    @Override
    public boolean onStartJob(JobParameters params) {
        MainAccessibilityService service = MainApplication.getService();
        if (service != null){
            PersistableBundle extras = params.getExtras();
            String id = extras.getString("id");
            Log.d("TAG", "onStartJob: " + id);
            service.runTask(TaskRepository.getInstance(service).getTaskById(id), null);
        }
        return false;
    }

    @Override
    public boolean onStopJob(JobParameters params) {
        return false;
    }
}
