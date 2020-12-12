package com.mirfatif.permissionmanagerx;

import android.app.Application;
import android.content.Context;
import android.os.SystemClock;
import android.util.Log;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

public class App extends Application {

  AppFlavor mAppFlavor;
  private static Context mAppContext;
  private Thread.UncaughtExceptionHandler defaultExceptionHandler;

  static final String crashLogDir = "crash_logs";
  static final String logFilePrefix = "PMX_";
  static final String logFileSuffix = ".log";

  @Override
  public void onCreate() {
    super.onCreate();
    mAppContext = getApplicationContext();
    defaultExceptionHandler = Thread.getDefaultUncaughtExceptionHandler();
    Thread.setDefaultUncaughtExceptionHandler(
        (t, e) -> {
          if (Utils.mLogcatWriter != null) {
            Log.e("App", e.toString());
            SystemClock.sleep(500);
            try {
              Utils.mLogcatWriter.flush();
            } catch (IOException ignored) {
            }
          }

          File logDir = new File(getExternalFilesDir(null), crashLogDir);
          if (logDir.exists() || logDir.mkdirs()) {
            String logFile = logFilePrefix + Utils.getCurrDateTime() + logFileSuffix;
            try {
              PrintWriter writer = new PrintWriter(new File(logDir, logFile));
              writer.println(Utils.getDeviceInfo());
              e.printStackTrace(writer);
              writer.close();
            } catch (FileNotFoundException ignored) {
            }
          } else {
            Log.e("App", "Failed to create " + logDir);
          }

          defaultExceptionHandler.uncaughtException(t, e);
        });
    mAppFlavor = new AppFlavor(this);
    mAppFlavor.onCreated();
  }

  public static Context getContext() {
    return mAppContext;
  }
}
