package com.mirfatif.permissionmanagerx.app;

import android.content.Context;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.os.SystemClock;
import com.mirfatif.permissionmanagerx.fwk.AppM;
import com.mirfatif.permissionmanagerx.util.AppLifecycle;
import com.mirfatif.permissionmanagerx.util.LocaleUtils;
import com.mirfatif.permissionmanagerx.util.LogUtils;
import com.mirfatif.privtasks.util.MyLog;
import java.io.PrintWriter;
import java.io.StringWriter;

public class App {

  private static final String TAG = "App";

  private final AppM mA;

  public App(AppM app) {
    mA = app;
  }

  private static Context sContext;

  private Thread.UncaughtExceptionHandler defaultExceptionHandler;

  public void onCreate() {
    sContext = mA.getApplicationContext();
    mPm = App.getCxt().getPackageManager();

    setLocale();

    defaultExceptionHandler = Thread.getDefaultUncaughtExceptionHandler();
    Thread.setDefaultUncaughtExceptionHandler(new ExceptionHandler());

    new AppFlavor().onCreated();

    AppLifecycle.init(mA);
  }

  public Configuration onConfigurationChanged(Configuration newConfig) {
    return LocaleUtils.setLocale(newConfig);
  }

  public static void setLocale() {
    LocaleUtils.setLocale(sContext);
  }

  public static Context getCxt() {

    while (sContext == null) {
      SystemClock.sleep(100);
    }
    return sContext;
  }

  public static Resources getRes() {
    return sContext.getResources();
  }

  private static PackageManager mPm;

  public static PackageManager getPm() {
    return mPm;
  }

  private class ExceptionHandler implements Thread.UncaughtExceptionHandler {

    public void uncaughtException(Thread t, Throwable e) {
      MyLog.e(TAG, "uncaughtException", t.getName() + ": " + e);

      StringWriter stringWriter = new StringWriter();
      PrintWriter writer = new PrintWriter(stringWriter, true);
      e.printStackTrace(writer);
      writer.close();
      LogUtils.showCrashNotification(stringWriter.toString(), false);

      defaultExceptionHandler.uncaughtException(t, e);
    }
  }
}
