package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.privtasks.util.bg.RunnableWithParam;
import com.mirfatif.privtasks.util.bg.SingleParamTask;

public class LiveSingleParamTask<T> extends SingleParamTask<T> {

  public LiveSingleParamTask(LifecycleOwner owner, RunnableWithParam<T> task, String threadName) {
    super(task, threadName);
    LifecycleWatcher.addOnDestroyed(owner, this::shutdownNow);
  }
}
