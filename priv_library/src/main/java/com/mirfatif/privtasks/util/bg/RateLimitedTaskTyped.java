package com.mirfatif.privtasks.util.bg;

import java.util.concurrent.TimeUnit;

public class RateLimitedTaskTyped<T> {

  private final RateLimiter mRateLimiter;
  private final RunnableWithParam<T> mTask;

  public RateLimitedTaskTyped(long minDelay, TimeUnit unit, RunnableWithParam<T> task) {
    mRateLimiter = new RateLimiter(minDelay, unit);
    mTask = task;
  }

  public void run(T param) {
    run(param, false);
  }

  public synchronized void run(T param, boolean force) {
    if (force || mRateLimiter.can()) {
      mTask.run(param);
      mRateLimiter.setTs();
    }
  }
}
