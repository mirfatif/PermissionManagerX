package com.mirfatif.privtasks.util.bg;

import static java.lang.System.currentTimeMillis;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

public class NotifyWaiter {

  private final AtomicBoolean WAITER = new AtomicBoolean(false);
  private final long mTimeout;
  private final Condition mCondition;

  public NotifyWaiter() {
    this(null);
  }

  public NotifyWaiter(long timeout, TimeUnit unit) {
    mTimeout = unit.toMillis(Math.max(0, timeout));
    mCondition = null;
  }

  public NotifyWaiter(Condition condition) {
    mTimeout = 0;
    mCondition = condition;
  }

  public void waitForNotifyNoThrow() {
    waitForNotifyNoThrow(mCondition);
  }

  public void waitForNotifyNoThrow(Condition condition) {
    try {
      waitForNotify(true, condition);
    } catch (InterruptedException ignored) {

    }
  }

  public void waitForNotify() throws InterruptedException {
    waitForNotify(mCondition);
  }

  public void waitForNotify(Condition condition) throws InterruptedException {
    waitForNotify(false, condition);
  }

  private void waitForNotify(boolean noThrow, Condition condition) throws InterruptedException {
    long ts = currentTimeMillis() + mTimeout, sleep = 0;

    try {
      synchronized (WAITER) {
        while (condition != null ? condition.shouldWait() : !WAITER.get()) {
          if (mTimeout > 0 && (sleep = ts - currentTimeMillis()) <= 0) {
            break;
          }

          try {
            WAITER.wait(sleep);
          } catch (InterruptedException e) {
            if (!noThrow) {
              throw e;
            }
          }
        }
      }
    } finally {
      WAITER.set(false);
    }
  }

  public void notify(boolean all) {
    synchronized (WAITER) {
      WAITER.set(true);

      if (all) {
        WAITER.notifyAll();
      } else {
        WAITER.notify();
      }
    }
  }

  public interface Condition {

    boolean shouldWait();
  }
}
