package com.mirfatif.privtasks.util.bg;

import android.os.Looper;
import java.util.concurrent.Executors;

public class ThreadUtils {

  private ThreadUtils() {}

  public static boolean isMainThread() {
    Looper looper = Looper.getMainLooper();
    if (looper == null) {
      return false;
    }
    return Thread.currentThread() == looper.getThread();
  }

  public static void assertNotMainThread() {
    if (isMainThread()) {
      throw new RuntimeException("Must not be called on main thread");
    }
  }

  public static Thread createDaemonThread(Runnable r) {
    Thread thread = Executors.defaultThreadFactory().newThread(r);
    thread.setDaemon(true);
    return thread;
  }
}
