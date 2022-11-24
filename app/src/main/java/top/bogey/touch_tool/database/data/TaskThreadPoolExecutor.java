package top.bogey.touch_tool.database.data;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

public class TaskThreadPoolExecutor extends ThreadPoolExecutor {
    private final AtomicInteger submittedTaskCount = new AtomicInteger(0);

    public TaskThreadPoolExecutor(int corePoolSize, int maximumPoolSize, long keepAliveTime, TimeUnit unit, TaskQueue<Runnable> workQueue) {
        super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue);
        workQueue.setExecutor(this);
    }

    public int getSubmittedTaskCount() {
        return submittedTaskCount.get();
    }

    @Override
    protected void afterExecute(Runnable r, Throwable t) {
        submittedTaskCount.decrementAndGet();
    }

    @Override
    public void execute(Runnable command) {
        if (command == null) throw new NullPointerException();
        submittedTaskCount.incrementAndGet();
        try {
            super.execute(command);
        } catch (RejectedExecutionException rx) {
            final TaskQueue<Runnable> queue = (TaskQueue<Runnable>) super.getQueue();
            try {
                if (!queue.retryOffer(command, 0, TimeUnit.MICROSECONDS)) {
                    submittedTaskCount.decrementAndGet();
                    throw new RejectedExecutionException("queue capacity is full", rx);
                }
            } catch (InterruptedException e) {
                submittedTaskCount.decrementAndGet();
                throw new RejectedExecutionException(e);
            }
        } catch (Throwable throwable) {
            submittedTaskCount.decrementAndGet();
        }
    }
}
