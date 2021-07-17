package com.mirfatif.permissionmanagerx.app;

import android.app.Application;
import android.content.Context;
import android.content.res.Resources;
import android.util.Log;
import com.mirfatif.permissionmanagerx.annot.SecurityLibBug;
import com.mirfatif.permissionmanagerx.svc.LogcatService;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.io.PrintWriter;
import java.io.StringWriter;

public class App extends Application {

  private static final String TAG = "App";

  @SuppressWarnings("FieldCanBeLocal")
  private AppFlavor mAppFlavor;

  private static Context mAppContext;
  private Thread.UncaughtExceptionHandler defaultExceptionHandler;

  @Override
  public void onCreate() {
    super.onCreate();
    mAppContext = getApplicationContext();
    defaultExceptionHandler = Thread.getDefaultUncaughtExceptionHandler();

    Thread.setDefaultUncaughtExceptionHandler(
        (t, e) -> {
          Log.e(TAG, e.toString());
          LogcatService.appCrashed();

          StringWriter stringWriter = new StringWriter();
          PrintWriter writer = new PrintWriter(stringWriter, true);
          e.printStackTrace(writer);
          writer.close();
          Utils.writeCrashLog(stringWriter.toString(), false);

          defaultExceptionHandler.uncaughtException(t, e);
        });

    // User is waiting for the first glance. Free the main thread.
    Utils.runInBg(
        () -> {
          mAppFlavor = new AppFlavor();
          mAppFlavor.onCreated();
        });

    Utils.runInBg(this::getEncPrefs);
  }

  public static Context getContext() {
    return mAppContext;
  }

  public static Resources getRes() {
    return mAppContext.getResources();
  }

  // To avoid delays later
  @SecurityLibBug
  private void getEncPrefs() {
    Utils.getEncPrefs();
  }
}
