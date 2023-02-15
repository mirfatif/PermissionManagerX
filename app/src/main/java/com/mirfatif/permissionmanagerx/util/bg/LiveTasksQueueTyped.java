package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;

public class LiveTasksQueueTyped<T> extends LiveTasksQueue {

  public LiveTasksQueueTyped(LifecycleOwner owner, BgTaskForResult<T> task) {
    super(owner, task);
  }

  protected void onDestroyed() {
    super.onDestroyed();
    mResult = null;
  }

  public LiveTasksQueueTyped<T> onUi(FgTask task) {
    super.onUi(task);
    return this;
  }

  public LiveTasksQueueTyped<T> onUiWith(FgTaskWithResult<T> task) {
    addTask(task);
    return this;
  }

  public LiveTasksQueueTyped<T> inBg(BgTask task) {
    super.inBg(task);
    return this;
  }

  public LiveTasksQueueTyped<T> inBgFor(BgTaskForResult<T> task) {
    addTask(task);
    return this;
  }

  public LiveTasksQueueTyped<T> inBgWith(BgTaskWithResult<T> task) {
    addTask(task);
    return this;
  }

  public LiveTasksQueueTyped<T> inBgWithFor(BgTaskWithForResult<T> task) {
    addTask(task);
    return this;
  }

  public LiveTasksQueueTyped<T> delay(long msDelay) {
    super.delay(msDelay);
    return this;
  }

  private volatile T mResult;

  public T getResult() {
    return mResult;
  }

  protected void run(Object task) {
    T result = mResult;

    if (task instanceof BgTaskForResult) {
      if (isAlive()) {
        mResult = ((BgTaskForResult<T>) task).run();
      }
    } else if (task instanceof BgTaskWithResult) {
      if (isAlive()) {
        ((BgTaskWithResult<T>) task).run(result);
      }
    } else if (task instanceof BgTaskWithForResult) {
      if (isAlive()) {
        mResult = ((BgTaskWithForResult<T>) task).run(result);
      }
    } else if (task instanceof FgTaskForResult) {
      if (isAlive()) {
        LiveUiWaitTask.post(() -> mResult = ((FgTaskForResult<T>) task).run()).waitForMe();
      }
    } else if (task instanceof FgTaskWithResult) {
      if (isAlive()) {
        LiveUiWaitTask.post(() -> ((FgTaskWithResult<T>) task).run(result)).waitForMe();
      }
    } else if (task instanceof FgTaskWithForResult) {
      if (isAlive()) {
        LiveUiWaitTask.post(
                () -> {
                  T res = mResult;
                  mResult = ((FgTaskWithForResult<T>) task).run(res);
                })
            .waitForMe();
      }
    } else {
      super.run(task);
    }
  }

  public interface BgTaskForResult<E> {

    E run();
  }

  public interface BgTaskWithResult<E> {

    void run(E result);
  }

  public interface BgTaskWithForResult<E> {

    E run(E result);
  }

  public interface FgTaskForResult<E> {

    E run();
  }

  public interface FgTaskWithResult<E> {

    void run(E result);
  }

  public interface FgTaskWithForResult<E> {

    E run(E result);
  }
}
