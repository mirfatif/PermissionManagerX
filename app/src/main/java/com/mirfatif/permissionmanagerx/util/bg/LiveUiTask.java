package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;

public class LiveUiTask {

  private Runnable mTask;

  public LiveUiTask(LifecycleOwner owner, Runnable task) {
    mTask = task;
    LifecycleWatcher.addOnDestroyed(owner, () -> mTask = null);
  }

  public void post() {
    post(false);
  }

  public void post(boolean waitForCompletion) {
    Runnable task = mTask;
    if (task == null) {
      return;
    }

    if (waitForCompletion) {
      LiveUiWaitTask.post(task).waitForMe();
    } else {
      UiRunner.post(task);
    }
  }
}
