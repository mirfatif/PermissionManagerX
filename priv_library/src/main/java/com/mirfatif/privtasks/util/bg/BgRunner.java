package com.mirfatif.privtasks.util.bg;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class BgRunner {

  private static final ExecutorService BG_EXECUTOR =
      Executors.newCachedThreadPool(ThreadUtils::createDaemonThread);

  public static void execute(Runnable task) {
    BG_EXECUTOR.execute(task);
  }

  public static <T> Future<T> submit(Callable<T> task) {
    return BG_EXECUTOR.submit(task);
  }
}
