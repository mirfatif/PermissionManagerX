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
            Log.e("App", e.toString());
            SystemClock.sleep(500);
            try {
              Utils.mLogcatWriter.flush();
            } catch (IOException ignored) {
            }
          }

          File logDir = new File(getExternalFilesDir(null), "crash_logs");
          if (logDir.exists() || logDir.mkdirs()) {
            String logFile = "PMX_" + Utils.getCurrDateTime() + ".log";
            try {
              PrintWriter writer = new PrintWriter(new File(logDir, logFile));
              e.printStackTrace(writer);
              writer.close();
            } catch (FileNotFoundException ignored) {
            }
          }

          defaultExceptionHandler.uncaughtException(t, e);
        });
  }

  static Context getContext() {
    return mAppContext;
  }
}
