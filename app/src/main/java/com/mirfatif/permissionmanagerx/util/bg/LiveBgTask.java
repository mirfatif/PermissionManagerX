package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.privtasks.util.bg.BgRunner;

public class LiveBgTask {

  private Runnable mTask;

  public LiveBgTask(LifecycleOwner owner, Runnable task) {
    mTask = task;
    LifecycleWatcher.addOnDestroyed(owner, () -> mTask = null);
  }

  public void execute() {
    Runnable task = mTask;
    if (task != null) {
      BgRunner.execute(task);
    }
  }
}
