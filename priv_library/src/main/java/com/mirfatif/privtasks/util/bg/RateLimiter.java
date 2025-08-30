package com.mirfatif.privtasks.util.bg;

import java.util.concurrent.TimeUnit;

public class RateLimiter {

  private final long mMinDelay;
  private long mTs;

  public RateLimiter(long minDelay, TimeUnit unit) {
    mMinDelay = unit.toMillis(minDelay);
  }

  public long getRemainingMillis() {
    return mTs + mMinDelay - System.currentTimeMillis();
  }

  public boolean can() {
    return can(false);
  }

  public boolean can(boolean setTs) {
    if (getRemainingMillis() <= 0) {
      if (setTs) {
        setTs();
      }
      return true;
    }
    return false;
  }

  public void setTs() {
    mTs = System.currentTimeMillis();
  }

  public void waitUntilCanNoThrow(boolean setTs) {
    try {
      waitUntilCan(setTs);
    } catch (InterruptedException ignored) {
    }
  }

  public void waitUntilCan(boolean setTs) throws InterruptedException {
    long sleep = getRemainingMillis();
    if (sleep > 0) {
      Thread.sleep(sleep);
    }
    if (setTs) {
      setTs();
    }
  }
}
