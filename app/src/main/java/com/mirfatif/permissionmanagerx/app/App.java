package com.mirfatif.permissionmanagerx.app;

import android.app.Application;
import android.content.Context;
import android.os.SystemClock;
import android.util.Log;
import com.mirfatif.permissionmanagerx.Utils;
import java.io.IOException;
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
          if (Utils.mLogcatWriter != null) {
            Log.e(TAG, e.toString());
            SystemClock.sleep(500);
            try {
              Utils.mLogcatWriter.flush();
            } catch (IOException ignored) {
            }
          }

          StringWriter stringWriter = new StringWriter();
          PrintWriter writer = new PrintWriter(stringWriter, true);
          e.printStackTrace(writer);
          writer.close();
          Utils.writeCrashLog(stringWriter.toString(), false);

          defaultExceptionHandler.uncaughtException(t, e);
        });

    mAppFlavor = new AppFlavor(this);
    mAppFlavor.onCreated();
  }

  public static Context getContext() {
    return mAppContext;
  }
}
