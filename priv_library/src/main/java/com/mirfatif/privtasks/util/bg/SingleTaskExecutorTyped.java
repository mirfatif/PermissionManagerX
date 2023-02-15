package com.mirfatif.privtasks.util.bg;

public class SingleTaskExecutorTyped<T> extends SingleTaskExecutor {

  private final RunnableWithResult<T> mTask;
  private final Runnable mTaskWrapper;
  private final T mDefValue;

  public SingleTaskExecutorTyped(
      RunnableWithResult<T> typedTask, T def, String threadName, int threadPriority) {
    super(typedTask::run, threadName, threadPriority);
    mTask = typedTask;
    mTaskWrapper = SingleTaskExecutorTyped.this::runTask;
    mDefValue = def;
  }

  public T cancelSubmitGet(boolean interrupt) {
    synchronized (mAlive) {
      if (mAlive.get()) {
        cancelAndAddTask(interrupt, mTaskWrapper, mQ);
      } else {
        return mDefValue;
      }

      return getResult();
    }
  }

  private final NotifyWaiter mDone = new NotifyWaiter();
  private volatile T mResult;

  private void runTask() {
    T res = mDefValue;
    try {
      res = mTask.run();
    } finally {
      mResult = res;
      mDone.notify(false);
    }
  }

  private T getResult() {
    try {
      mDone.waitForNotify();
      return mResult;
    } catch (InterruptedException e) {
      return mDefValue;
    } finally {
      mResult = null;
    }
  }
}
