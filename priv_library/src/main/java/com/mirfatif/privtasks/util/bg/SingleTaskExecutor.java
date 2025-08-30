package com.mirfatif.privtasks.util.bg;

import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

public class SingleTaskExecutor extends SingleTaskExecutorAbstract {

  protected final ConcurrentLinkedQueue<Runnable> mQ = new ConcurrentLinkedQueue<>();

  private final Runnable mTask;

  public SingleTaskExecutor(Runnable task, String threadName) {
    this(task, threadName, -1);
  }

  public SingleTaskExecutor(Runnable task, String threadName, int threadPriority) {
    super(threadName, threadPriority);
    mTask = task;
    start();
  }

  protected Runnable poll() {
    return mQ.poll();
  }

  protected long getTimeoutMillis() {
    return 0;
  }

  protected Queue<?> getQueue() {
    return mQ;
  }

  public void submit() {
    synchronized (mAlive) {
      if (mAlive.get()) {
        addTask(mTask, mQ);
      }
    }
  }

  public void cancelAndSubmit(boolean interrupt) {
    synchronized (mAlive) {
      if (mAlive.get()) {
        cancelAndAddTask(interrupt, mTask, mQ);
      }
    }
  }
}
