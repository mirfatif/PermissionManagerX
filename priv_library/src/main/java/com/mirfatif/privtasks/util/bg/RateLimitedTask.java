package com.mirfatif.privtasks.util.bg;

import java.util.concurrent.TimeUnit;

public class RateLimitedTask {

  private final RateLimiter mRateLimiter;
  private final Runnable mTask;

  public RateLimitedTask(long minDelay, TimeUnit unit, Runnable task) {
    mRateLimiter = new RateLimiter(minDelay, unit);
    mTask = () -> RateLimitedTask.this.run(task);
  }

  public void run() {
    run(false);
  }

  public void run(boolean force) {
    run(force, false);
  }

  private boolean mRunning = false;

  public synchronized void run(boolean force, boolean inBg) {
    if (force || mRateLimiter.can() && !mRunning) {
      mRunning = true;

      if (inBg) {
        BgRunner.execute(mTask);
      } else {
        mTask.run();
      }
    }
  }

  private void run(Runnable task) {
    task.run();
    mRateLimiter.setTs();
    mRunning = false;
  }
}
