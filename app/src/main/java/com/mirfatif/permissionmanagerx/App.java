package com.mirfatif.permissionmanagerx;

import android.app.Activity;
import android.app.ActivityManager;
import android.app.Application;
import android.content.ComponentCallbacks2;
import android.content.Context;
import android.content.res.Configuration;
import android.os.Bundle;
import android.os.SystemClock;
import android.util.Log;
import android.widget.Toast;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.privdaemon.Util;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

public class App extends Application {

  static final String TAG = "App";

  AppFlavor mAppFlavor;
  private static Context mAppContext;
  private Thread.UncaughtExceptionHandler defaultExceptionHandler;

  private MySettings mMySettings;

  @Override
  public void onCreate() {
    super.onCreate();
    mAppContext = getApplicationContext();
    defaultExceptionHandler = Thread.getDefaultUncaughtExceptionHandler();
    mMySettings = MySettings.getInstance();

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

          File logDir = Utils.createCrashLogDir();
          if (logDir != null) {
            try {
              PrintWriter writer = new PrintWriter(Util.getCrashLogFile(logDir.toString(), false));
              writer.println(Util.getDeviceInfo());
              e.printStackTrace(writer);
              writer.close();
            } catch (FileNotFoundException ignored) {
            }
          }

          defaultExceptionHandler.uncaughtException(t, e);
        });

    registerComponentCallbacks(
        new ComponentCallbacks2() {
          @Override
          public void onTrimMemory(int level) {
            String memLevel = "Unknown";
            boolean isRunning = false, isCritical = false;
            switch (level) {
              case ComponentCallbacks2.TRIM_MEMORY_RUNNING_MODERATE:
                memLevel = "TRIM_MEMORY_RUNNING_MODERATE";
                isRunning = true;
                break;
              case ComponentCallbacks2.TRIM_MEMORY_RUNNING_LOW:
                memLevel = "TRIM_MEMORY_RUNNING_LOW";
                isRunning = true;
                break;
              case ComponentCallbacks2.TRIM_MEMORY_RUNNING_CRITICAL:
                memLevel = "TRIM_MEMORY_RUNNING_CRITICAL";
                isRunning = isCritical = true;
                break;
              case ComponentCallbacks2.TRIM_MEMORY_UI_HIDDEN:
                memLevel = "TRIM_MEMORY_UI_HIDDEN";
                break;
              case ComponentCallbacks2.TRIM_MEMORY_BACKGROUND:
                memLevel = "TRIM_MEMORY_BACKGROUND";
                break;
              case ComponentCallbacks2.TRIM_MEMORY_MODERATE:
                memLevel = "TRIM_MEMORY_MODERATE";
                break;
              case ComponentCallbacks2.TRIM_MEMORY_COMPLETE:
                memLevel = "TRIM_MEMORY_COMPLETE";
                break;
            }

            if (isCritical) {
              mMySettings.setLowMemory(true);
              PackageParser.getInstance().releaseIcons();
              Toast.makeText(App.this, R.string.device_low_memory, Toast.LENGTH_LONG).show();
            }
            if (mMySettings.DEBUG) {
              Utils.debugLog(TAG, "onTrimMemory level: " + memLevel);
            } else if (isRunning) {
              Log.i(TAG, "onTrimMemory level: " + memLevel);
            }
          }

          @Override
          public void onConfigurationChanged(Configuration newConfig) {}

          @Override
          public void onLowMemory() {}
        });

    registerActivityLifecycleCallbacks(
        new ActivityLifecycleCallbacks() {
          @Override
          public void onActivityCreated(Activity activity, Bundle savedInstanceState) {}

          @Override
          public void onActivityStarted(Activity activity) {
            if (activity instanceof MainActivity) {
              ActivityManager am = (ActivityManager) App.this.getSystemService(ACTIVITY_SERVICE);
              ActivityManager.MemoryInfo memInfo = new ActivityManager.MemoryInfo();
              am.getMemoryInfo(memInfo);
              mMySettings.setLowMemory(memInfo.lowMemory);
              if (mMySettings.DEBUG) {
                Utils.debugLog(TAG, "Low on memory: " + memInfo.lowMemory);
              } else if (memInfo.lowMemory) {
                Log.i(TAG, "Low on memory: " + true);
              }
            }
          }

          @Override
          public void onActivityResumed(Activity activity) {}

          @Override
          public void onActivityPaused(Activity activity) {}

          @Override
          public void onActivityStopped(Activity activity) {}

          @Override
          public void onActivitySaveInstanceState(Activity activity, Bundle outState) {}

          @Override
          public void onActivityDestroyed(Activity activity) {}
        });

    mAppFlavor = new AppFlavor(this);
    mAppFlavor.onCreated();
  }

  public static Context getContext() {
    return mAppContext;
  }
}
