package com.mirfatif.privtasks.util.bg;

import java.util.Queue;
import java.util.concurrent.atomic.AtomicBoolean;

public abstract class SingleTaskExecutorAbstract {

  protected final Thread mThread;

  protected SingleTaskExecutorAbstract(String threadName) {
    this(threadName, -1);
  }

  protected SingleTaskExecutorAbstract(String threadName, int threadPriority) {
    mThread =
        threadName != null ? new Thread(this::runLoop, threadName) : new Thread(this::runLoop);
    mThread.setDaemon(true);
    if (threadPriority >= Thread.MIN_PRIORITY && threadPriority <= Thread.MAX_PRIORITY) {
      mThread.setPriority(threadPriority);
    }
  }

  protected void start() {
    mThread.start();
  }

  protected void finalize() {
    shutdownNow();
  }

  protected abstract Runnable poll();

  protected abstract long getTimeoutMillis();

  protected abstract Queue<?> getQueue();

  protected final AtomicBoolean mAlive = new AtomicBoolean(true);

  private final AtomicBoolean mRunning = new AtomicBoolean(false);

  private void runLoop() {
    Runnable task;
    long timeout;

    while (mAlive.get()) {
      synchronized (mRunning) {
        while ((task = poll()) == null) {
          if (!mAlive.get()) {
            return;
          }

          timeout = getTimeoutMillis();
          if (timeout < 0) {

            continue;
          }

          try {
            mRunning.wait(timeout);
          } catch (InterruptedException ignored) {
          }
        }

        mRunning.set(true);
      }

      task.run();

      synchronized (mRunning) {
        clearInterrupt();
        mRunning.set(false);
      }
    }
  }

  private static void clearInterrupt() {
    Thread.interrupted();
  }

  private void clearQueue(boolean interrupt) {
    getQueue().clear();
    if (interrupt) {
      synchronized (mRunning) {
        if (mRunning.get()) {
          mThread.interrupt();
        }
      }
    }
  }

  protected <T> void cancelAndAddTask(boolean interrupt, T task, Queue<T> queue) {
    clearQueue(interrupt);
    addTask(task, queue);
  }

  protected <T> void addTask(T task, Queue<T> queue) {
    synchronized (mRunning) {
      queue.add(task);
      mRunning.notifyAll();
    }
  }

  public boolean isAlive() {
    return mAlive.get();
  }

  public boolean hasPendingTasks() {
    return !getQueue().isEmpty();
  }

  public boolean hasRunningOrPendingTasks() {
    return mRunning.get() || hasPendingTasks();
  }

  public void cancel(boolean interrupt) {
    synchronized (mAlive) {
      if (mAlive.get()) {
        clearQueue(interrupt);
      }
    }
  }

  public void shutdownNow() {
    synchronized (mAlive) {
      mAlive.set(false);
      getQueue().clear();
      mThread.interrupt();
    }
  }
}
