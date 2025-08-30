package com.mirfatif.privtasks.util.bg;

import java.util.concurrent.ConcurrentLinkedQueue;

public class SingleParamTask<T> {

  private final ConcurrentLinkedQueue<T> mParam = new ConcurrentLinkedQueue<>();
  private final RunnableWithParam<T> mTask;
  private final SingleTaskExecutor mE;

  public SingleParamTask(RunnableWithParam<T> task, String threadName) {
    mTask = task;
    mE = new SingleTaskExecutor(this::runPendingParams, threadName);
  }

  private void runPendingParams() {
    T param;
    while ((param = mParam.poll()) != null) {
      mTask.run(param);
    }
  }

  private void enqueueParam(T param) {
    mParam.add(param);
    if (!hasRunningOrPendingTasks()) {
      mE.submit();
    }
  }

  public synchronized void submitIfIdle(T param) {
    if (mE.isAlive() && !hasRunningOrPendingTasks() && mParam.isEmpty()) {
      enqueueParam(param);
    }
  }

  public synchronized void cancelAndSubmit(T param, boolean interrupt) {
    if (mE.isAlive()) {
      cancel(interrupt);
      enqueueParam(param);
    }
  }

  public void cancel(boolean interrupt) {
    mParam.clear();
    mE.cancel(interrupt);
  }

  public synchronized void shutdownNow() {
    mE.shutdownNow();
  }

  public boolean hasRunningOrPendingTasks() {
    return mE.hasRunningOrPendingTasks();
  }
}
