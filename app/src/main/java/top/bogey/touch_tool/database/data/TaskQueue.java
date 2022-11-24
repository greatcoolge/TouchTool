package top.bogey.touch_tool.database.data;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;

public class TaskQueue<Runnable> extends LinkedBlockingQueue<Runnable> {

    private TaskThreadPoolExecutor executor;

    public TaskQueue(int capacity) {
        super(capacity);
    }

    public void setExecutor(TaskThreadPoolExecutor executor) {
        this.executor = executor;
    }

    @Override
    public boolean offer(Runnable runnable) {
        if (executor == null) throw new RejectedExecutionException("does not have executor");

        int currPoolSize = executor.getPoolSize();
        // 提交数小于线程池，直接入队，因为有空闲线程，会被快速执行
        if (executor.getSubmittedTaskCount() < currPoolSize) {
            return super.offer(runnable);
        }

        // 当前池小于最大池，入队失败，去新建线程
        if (currPoolSize < executor.getMaximumPoolSize()) return false;

        // 达到最大池了，入队吧
        return super.offer(runnable);
    }

    public boolean retryOffer(Runnable runnable, long timeout, TimeUnit unit) throws InterruptedException {
        if (executor.isShutdown()) throw new RejectedExecutionException("executor is shutdown");
        return super.offer(runnable, timeout, unit);
    }
}
