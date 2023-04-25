package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.privtasks.util.bg.BgRunner;
import com.mirfatif.privtasks.util.bg.SingleSchedTaskExecutor;
import java.util.concurrent.TimeUnit;

public class LiveSchedTask {

  private final SingleSchedTaskExecutor mE;
  private final long mDelayMillis;
  private final boolean mUi;

  public LiveSchedTask(
      LifecycleOwner owner,
      Runnable task,
      long delay,
      TimeUnit unit,
      boolean onUi,
      String threadName) {
    this(owner, task, delay, unit, onUi, false, threadName);
  }

  private LiveSchedTask(
      LifecycleOwner owner,
      Runnable task,
      long delay,
      TimeUnit unit,
      boolean onUi,
      boolean oneShot,
      String threadName) {
    Task callback = new Task(oneShot ? () -> runAndShutdown(task) : task);
    LifecycleWatcher.addOnDestroyed(owner, callback);

    mE = new SingleSchedTaskExecutor(callback, threadName);
    mDelayMillis = unit.toMillis(delay);
    mUi = onUi;

    if (oneShot) {
      mE.schedule(mDelayMillis, TimeUnit.MILLISECONDS);
    }
  }

  public void cancelAndSchedule() {
    mE.cancelAndSchedule(true, mDelayMillis, TimeUnit.MILLISECONDS);
  }

  public void cancel() {
    mE.cancel(true);
  }

  private void runAndShutdown(Runnable task) {
    task.run();
    shutdownNow();
  }

  public boolean isAlive() {
    return mE.isAlive();
  }

  public void shutdownNow() {
    mE.shutdownNow();
  }

  private class Task implements Runnable, LifecycleWatcher.LifecycleCallback {

    private Runnable mTask;

    private Task(Runnable task) {
      mTask = task;
    }

    public void run() {
      Runnable task = mTask;
      if (task != null) {
        if (mUi) {
          LiveUiWaitTask.post(task).waitForMe();
        } else {
          task.run();
        }
      }
    }

    public void onDestroyed() {
      mTask = null;
      BgRunner.execute(LiveSchedTask.this::shutdownNow);
    }
  }

  public static void schedule(
      LifecycleOwner owner,
      Runnable task,
      long delay,
      TimeUnit unit,
      boolean onUi,
      String threadName) {
    new LiveSchedTask(owner, task, delay, unit, onUi, true, threadName);
  }
}
