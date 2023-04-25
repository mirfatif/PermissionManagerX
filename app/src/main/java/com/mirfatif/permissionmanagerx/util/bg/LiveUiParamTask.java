package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.privtasks.util.bg.RunnableWithParam;
import java.util.concurrent.atomic.AtomicReference;

public class LiveUiParamTask<T> {

  private final AtomicReference<T> mParam = new AtomicReference<>();
  private Runnable mTask;

  public LiveUiParamTask(LifecycleOwner owner, RunnableWithParam<T> task) {
    mTask = () -> runWithParam(task);
    LifecycleWatcher.addOnDestroyed(owner, this::stop);
  }

  private void runWithParam(RunnableWithParam<T> task) {
    task.run(mParam.get());
  }

  public void post(T param) {
    post(param, false);
  }

  public void post(T param, boolean waitForCompletion) {
    Runnable task = mTask;

    if (task == null) {
      return;
    }

    mParam.set(param);

    if (waitForCompletion) {
      LiveUiWaitTask.post(task).waitForMe();
    } else {
      UiRunner.post(task);
    }
  }

  public void stop() {
    mTask = null;
  }
}
