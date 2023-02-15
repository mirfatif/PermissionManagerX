package com.mirfatif.permissionmanagerx.util.bg;

import android.os.Handler;
import android.os.Looper;
import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.privtasks.util.bg.ThreadUtils;

public class UiRunner {

  private static final Handler MAIN_HANDLER = new Handler(Looper.getMainLooper());

  public static void post(Runnable task) {
    if (ThreadUtils.isMainThread()) {
      task.run();
    } else {
      MAIN_HANDLER.post(task);
    }
  }

  public static void post(LifecycleOwner owner, Runnable task) {
    new LiveUiTask(owner, task).post();
  }
}
