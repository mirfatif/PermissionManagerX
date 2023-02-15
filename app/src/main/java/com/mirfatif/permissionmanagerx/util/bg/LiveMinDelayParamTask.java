package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.privtasks.util.bg.MinDelayTaskExecutor;
import com.mirfatif.privtasks.util.bg.RunnableWithParam;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class LiveMinDelayParamTask<T> {

  private final MinDelayTaskExecutor mE;

  public LiveMinDelayParamTask(
      LifecycleOwner owner,
      RunnableWithParam<T> task,
      long delay,
      TimeUnit unit,
      boolean onUi,
      String threadName) {
    LifecycleWatcher.addOnDestroyed(owner, this::shutdownNow);
    mE =
        new MinDelayTaskExecutor(
            onUi
                ? () -> LiveUiWaitTask.post(() -> runWithParam(task)).waitForMe()
                : () -> runWithParam(task),
            delay,
            unit,
            threadName);
  }

  public void cancelAndRunOrSchedule(T param) {
    cancelAndRunOrSchedule(param, false);
  }

  public void cancelAndRunNow(T param) {
    cancelAndRunOrSchedule(param, true);
  }

  private final AtomicReference<T> mParam = new AtomicReference<>();

  private void cancelAndRunOrSchedule(T param, boolean runNow) {
    synchronized (this) {
      if (!mE.isAlive()) {
        return;
      }

      mParam.set(param);

      if (runNow) {
        mE.cancelAndRunNow();
      } else {
        mE.runOrSchedule(true);
      }
    }
  }

  public void shutdownNow() {
    synchronized (this) {
      mParam.set(null);
      mE.shutdownNow();
    }
  }

  private void runWithParam(RunnableWithParam<T> task) {
    T param = mParam.getAndSet(null);
    if (param != null) {
      task.run(param);
    }
  }
}
