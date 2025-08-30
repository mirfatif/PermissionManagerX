package com.mirfatif.privtasks.util.bg;

import java.util.concurrent.TimeUnit;

public class MinDelayTaskExecutor {

  private final RateLimiter mRateLimiter;
  private final SingleSchedTaskExecutor mE;

  public MinDelayTaskExecutor(Runnable task, long delay, TimeUnit unit, String threadName) {
    mRateLimiter = new RateLimiter(delay, unit);
    mE = new SingleSchedTaskExecutor(() -> run(task), threadName);
  }

  public void runOrSchedule(boolean cancel) {
    synchronized (this) {
      if (!mE.isAlive() || (!cancel && mE.hasRunningOrPendingTasks())) {
        return;
      }

      if (cancel) {
        mE.cancel(true);
      }

      runNow(mRateLimiter.getRemainingMillis());
    }
  }

  public void cancelAndRunNow() {
    synchronized (this) {
      if (mE.isAlive()) {
        mE.cancel(true);
        runNow(0);
      }
    }
  }

  private void runNow(long delay) {
    mE.scheduleIfIdle(delay > 0 ? delay : 0, TimeUnit.MILLISECONDS);
  }

  public void shutdownNow() {
    mE.shutdownNow();
  }

  public boolean isAlive() {
    return mE.isAlive();
  }

  private void run(Runnable task) {
    task.run();
    mRateLimiter.setTs();
  }
}
