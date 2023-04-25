package com.mirfatif.privtasks.util.bg;

import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static java.util.concurrent.TimeUnit.NANOSECONDS;

import java.util.PriorityQueue;
import java.util.Queue;
import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

public class SingleSchedTaskExecutor extends SingleTaskExecutorAbstract {

  private final PriorityQueue<DelayedRunnable> mQ = new PriorityQueue<>();

  private final DelayedRunnable mTask;

  public SingleSchedTaskExecutor(Runnable task, String threadName) {
    this(task, false, threadName);
  }

  private SingleSchedTaskExecutor(Runnable task, boolean oneshot, String threadName) {
    super(threadName);
    mTask = new DelayedRunnable(oneshot ? () -> runAndShutdown(task) : task);
    start();
  }

  private void runAndShutdown(Runnable task) {
    task.run();
    shutdownNow();
  }

  protected Runnable poll() {
    DelayedRunnable task = mQ.peek();
    return (task == null || task.getDelay(MILLISECONDS) > 0) ? null : mQ.poll();
  }

  protected long getTimeoutMillis() {
    DelayedRunnable task = mQ.peek();
    return task == null ? 0 : task.getDelay(MILLISECONDS);
  }

  protected Queue<?> getQueue() {
    return mQ;
  }

  public void schedule(long delay, TimeUnit unit) {
    synchronized (mAlive) {
      if (mAlive.get()) {
        addTask(mTask.setTime(unit, delay), mQ);
      }
    }
  }

  public void scheduleIfIdle(long delay, TimeUnit unit) {
    if (!hasRunningOrPendingTasks()) {
      schedule(delay, unit);
    }
  }

  public void cancelAndSchedule(boolean interrupt, long delay, TimeUnit unit) {
    synchronized (mAlive) {
      if (mAlive.get()) {
        cancelAndAddTask(interrupt, mTask.setTime(unit, delay), mQ);
      }
    }
  }

  public static void schedule(Runnable task, long delay, TimeUnit unit, String name) {
    new SingleSchedTaskExecutor(task, true, name).schedule(delay, unit);
  }

  private static class DelayedRunnable implements Runnable, Delayed {

    private final Runnable task;

    public DelayedRunnable(Runnable task) {
      this.task = task;
    }

    public void run() {
      task.run();
    }

    private final AtomicLong ts = new AtomicLong();

    private DelayedRunnable setTime(TimeUnit unit, long delay) {
      ts.set(System.nanoTime() + unit.toNanos(Math.max(0, delay)));
      return this;
    }

    public int compareTo(Delayed other) {
      if (other == this) {
        return 0;
      }

      long diff = getDelay(NANOSECONDS) - other.getDelay(NANOSECONDS);
      return diff < 0 ? -1 : diff > 0 ? 1 : 0;
    }

    public long getDelay(TimeUnit unit) {
      return unit.convert(ts.get() - System.nanoTime(), NANOSECONDS);
    }
  }
}
