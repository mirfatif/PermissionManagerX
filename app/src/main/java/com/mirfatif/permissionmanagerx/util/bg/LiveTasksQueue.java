package com.mirfatif.permissionmanagerx.util.bg;

import android.os.SystemClock;
import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.privtasks.util.bg.BgRunner;
import com.mirfatif.privtasks.util.bg.ThreadUtils;
import java.util.concurrent.ConcurrentLinkedQueue;

public class LiveTasksQueue {

  protected LiveTasksQueue(LifecycleOwner owner, Object task) {
    addTask(task);
    LifecycleWatcher.addOnDestroyed(owner, () -> BgRunner.execute(this::onDestroyed));
  }

  public LiveTasksQueue(LifecycleOwner owner, BgTask task) {
    this(owner, (Object) task);
  }

  public LiveTasksQueue(LifecycleOwner owner, long msDelay) {
    this(owner, () -> SystemClock.sleep(msDelay));
  }

  protected void onDestroyed() {
    synchronized (mTasks) {
      mTasks.clear();
      mAlive = false;
    }
  }

  protected boolean isAlive() {
    return mAlive;
  }

  public LiveTasksQueue onUi(FgTask task) {
    addTask(task);
    return this;
  }

  public LiveTasksQueue delay(long msDelay) {
    addTask((BgTask) () -> SystemClock.sleep(msDelay));
    return this;
  }

  private final ConcurrentLinkedQueue<Object> mTasks = new ConcurrentLinkedQueue<>();

  protected void addTask(Object task) {
    assertNotRunning();

    synchronized (mTasks) {
      if (mAlive) {
        mTasks.add(task);
      }
    }
  }

  private void assertNotRunning() {
    synchronized (mTasks) {
      if (mRunning) {
        throw new RuntimeException("Cannot add task after calling start()");
      }
    }
  }

  private boolean mAlive = true, mRunning = false;

  public void start() {
    start(false);
  }

  public void start(boolean blockedIfInBg) {
    if (!blockedIfInBg || ThreadUtils.isMainThread()) {
      BgRunner.execute(this::startBlocked);
    } else {
      startBlocked();
    }
  }

  public void startBlocked() {
    if (ThreadUtils.isMainThread()) {
      throw new RuntimeException("startBlocked() called on main thread");
    }

    synchronized (mTasks) {
      if (mRunning) {
        throw new RuntimeException("Already started");
      }
      mRunning = true;
    }

    if (mAlive) {
      run();
    }
  }

  private void run() {
    Object task;
    while (true) {
      synchronized (mTasks) {
        task = mTasks.poll();
      }
      if (task == null) {
        break;
      }
      run(task);
    }
  }

  protected void run(Object task) {
    if (task instanceof FgTask) {
      LiveUiWaitTask.post((Runnable) task).waitForMe();
    } else {
      ((BgTask) task).run();
    }
  }

  public interface BgTask extends Runnable {}

  public interface FgTask extends Runnable {}
}
