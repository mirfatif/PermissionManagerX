package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.privtasks.util.bg.NotifyWaiter;
import com.mirfatif.privtasks.util.bg.ThreadUtils;

public class LiveUiWaitTask {

  private LiveUiWaitTask(LifecycleOwner owner, Runnable task) {
    LiveWaitTask liveWaitTask = new LiveWaitTask(task);
    if (owner != null) {
      LifecycleWatcher.addOnDestroyed(owner, liveWaitTask);
    }
    UiRunner.post(liveWaitTask);
  }

  public static LiveUiWaitTask post(Runnable task) {
    return new LiveUiWaitTask(null, task);
  }

  private final NotifyWaiter mCompletionWaiter = new NotifyWaiter();

  public void waitForMe() {
    if (ThreadUtils.isMainThread()) {
      throw new RuntimeException("waitForMe() called on main thread");
    }
    mCompletionWaiter.waitForNotifyNoThrow();
  }

  private class LiveWaitTask implements Runnable, LifecycleWatcher.LifecycleCallback {

    private Runnable mTask;

    private LiveWaitTask(Runnable task) {
      mTask = task;
    }

    public void run() {
      Runnable task = mTask;
      if (task != null) {
        task.run();
      }
      mCompletionWaiter.notify(true);
    }

    public void onDestroyed() {
      mTask = null;
    }
  }
}
