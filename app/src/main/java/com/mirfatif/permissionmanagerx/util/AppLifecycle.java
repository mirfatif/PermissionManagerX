package com.mirfatif.permissionmanagerx.util;

import android.app.Activity;
import android.app.Application;
import android.os.Bundle;
import com.mirfatif.privtasks.util.bg.SingleSchedTaskExecutor;
import java.util.concurrent.TimeUnit;

public class AppLifecycle {

  private static final String TAG = "AppLifecycle";

  private AppLifecycle() {}

  private static int sAlive, sVisible;

  public static boolean isAppInFg() {
    return sAlive > 0;
  }

  public static boolean isAppVisible() {
    return sVisible > 0;
  }

  public static void init(Application app) {
    app.registerActivityLifecycleCallbacks(new Callbacks());
  }

  private static class Callbacks implements Application.ActivityLifecycleCallbacks {

    private final SingleSchedTaskExecutor mLiveRemover =
        new SingleSchedTaskExecutor(() -> sAlive--, TAG + "-LiveRemover");

    private final SingleSchedTaskExecutor mVisibleRemover =
        new SingleSchedTaskExecutor(() -> sVisible--, TAG + "-VisibleRemover");

    public void onActivityCreated(Activity activity, Bundle savedInstanceState) {
      sAlive++;
    }

    public void onActivityStarted(Activity activity) {
      sVisible++;
    }

    public void onActivityResumed(Activity activity) {}

    public void onActivityPaused(Activity activity) {}

    public void onActivityStopped(Activity activity) {
      mVisibleRemover.schedule(2, TimeUnit.SECONDS);
    }

    public void onActivitySaveInstanceState(Activity activity, Bundle outState) {}

    public void onActivityDestroyed(Activity activity) {
      mLiveRemover.schedule(2, TimeUnit.SECONDS);
    }
  }
}
