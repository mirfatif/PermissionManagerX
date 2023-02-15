package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.privtasks.util.bg.RunnableWithParam;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class LiveSchedParamTask<T> {

  private final LiveSchedTask mTask;

  public LiveSchedParamTask(
      LifecycleOwner owner,
      RunnableWithParam<T> task,
      long delay,
      TimeUnit unit,
      boolean onUi,
      String threadName) {
    mTask = new LiveSchedTask(owner, () -> runWithParam(task), delay, unit, onUi, threadName);
  }

  private void runWithParam(RunnableWithParam<T> task) {
    T param;
    synchronized (mParam) {
      param = mParam.getAndSet(null);
    }
    if (param != null && !Thread.interrupted()) {
      task.run(param);
    }
  }

  private final AtomicReference<T> mParam = new AtomicReference<>();

  public void cancelAndSchedule(T param) {
    if (mTask.isAlive()) {
      synchronized (mParam) {
        mParam.set(param);
        mTask.cancelAndSchedule();
      }
    }
  }

  public void cancel() {
    if (mTask.isAlive()) {
      synchronized (mParam) {
        mParam.set(null);
        mTask.cancel();
      }
    }
  }
}
