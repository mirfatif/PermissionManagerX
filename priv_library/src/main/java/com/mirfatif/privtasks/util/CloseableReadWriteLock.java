package com.mirfatif.privtasks.util;

import com.mirfatif.privtasks.util.bg.RunnableWithResult;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class CloseableReadWriteLock extends ReentrantReadWriteLock {

  public void withReadLock(Runnable task) {
    super.readLock().lock();
    try {
      task.run();
    } finally {
      super.readLock().unlock();
    }
  }

  public void withWriteLock(Runnable task) {
    super.writeLock().lock();
    try {
      task.run();
    } finally {
      super.writeLock().unlock();
    }
  }

  public <T> T withReadLock(RunnableWithResult<T> task) {
    super.readLock().lock();
    try {
      return task.run();
    } finally {
      super.readLock().unlock();
    }
  }

  public <T> T withWriteLock(RunnableWithResult<T> task) {
    super.writeLock().lock();
    try {
      return task.run();
    } finally {
      super.writeLock().unlock();
    }
  }
}
