package com.mirfatif.privdaemon;

import android.content.Intent;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Process;
import android.os.RemoteException;
import com.mirfatif.err.HiddenAPIsException;
import com.mirfatif.privtasks.Constants;
import com.mirfatif.privtasks.bind.ILogCallback;
import com.mirfatif.privtasks.bind.IPrivTasksCallback;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;
import com.mirfatif.privtasks.iface.IPrivTasks;
import com.mirfatif.privtasks.util.LogUtil;
import com.mirfatif.privtasks.util.Util;
import com.mirfatif.privtasks.util.bg.SingleTaskExecutor;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.Thread.UncaughtExceptionHandler;

public enum Callbacks {
  INS;

  private static final String TAG = "Callbacks";

  private String mAppId, mBinderSvc;
  private IPrivTasks mIPrivTasks;

  void talkToApp(String appId, String binderSvc, String codeWord) throws RemoteException {
    mAppId = appId;
    mBinderSvc = binderSvc;
    mIPrivTasks = new IPrivTasksImpl();

    UncaughtExceptionHandler defExcHandler = Thread.getDefaultUncaughtExceptionHandler();
    Thread.setDefaultUncaughtExceptionHandler(
        (t, e) -> {
          DaemonLog.e(TAG, "UncaughtExceptionHandler", e);
          saveCrashLog(e);
          if (defExcHandler != null) {
            defExcHandler.uncaughtException(t, e);
          }
        });

    talkToApp(codeWord);
  }

  void talkToApp(String codeWord) throws RemoteException {
    Bundle bundle = new Bundle();
    bundle.putBinder(Constants.EXTRA_BINDER, (IBinder) mIPrivTasks);

    Intent intent = new Intent();
    intent.putExtra(Constants.EXTRA_CODE_WORD, codeWord);
    intent.putExtra(Constants.EXTRA_BINDER, bundle);
    fireSvcIntent(intent, mBinderSvc, false);
  }

  void sendStdErr(int port) throws HiddenAPIsException {
    if (!Jni.INS.sendStdErr(port)) {
      throw new HiddenAPIsException("Filed to redirect STDERR");
    }
  }

  private IPrivTasksCallback mCallback;
  private String mCrashLogFile;

  void update(IPrivTasksCallback callback, String crashLogFile) throws RemoteException {
    mCallback = callback;
    mCrashLogFile = crashLogFile;

    mCallback.asBinder().linkToDeath(this::onAppDied, 0);
  }

  private boolean mExitOnAppDeath = false;

  void setExitOnAppDeath(boolean exitOnAppDeath) {
    mExitOnAppDeath = exitOnAppDeath;
  }

  private void onAppDied() {
    Jni.INS.closeStdErr();

    if (mExitOnAppDeath) {
      DaemonLog.i(TAG, "onAppDied", "Exiting...");
      exit();
    }
  }

  void exit() {
    System.err.close();

    DaemonLog.i(TAG, "exit", "Bye bye from PID " + Process.myPid());
    System.exit(0);
  }

  void showError(int error) {
    if (mCallback != null) {
      try {
        mCallback.showError(error);
      } catch (RemoteException e) {
        DaemonLog.e(TAG, "showError", e);
      }
    }
  }

  void saveCrashLog(Throwable e) {
    StringWriter writer = new StringWriter();
    e.printStackTrace(new PrintWriter(writer));
    String stackTrace = writer.toString();

    if (mCallback != null) {
      try {
        mCallback.saveLog(stackTrace);
      } catch (RemoteException re) {
        DaemonLog.e(TAG, "saveCrashLog", re);
        try {
          LogUtil.writeCrashLog(new File(mCrashLogFile), getDaemonState(), stackTrace, true);
        } catch (IOException ioe) {
          DaemonLog.e(TAG, "saveCrashLog", ioe);
        }
      }
    }
  }

  private static String getDaemonState() {
    return "\nUID: " + Process.myUid() + "\nContext: " + getSEContext();
  }

  static String getSEContext() {
    return Util.readNullTermFile("/proc/" + Process.myPid() + "/attr/current");
  }

  public void fireSvcIntent(Intent intent, String svcClass, boolean fg) throws RemoteException {
    intent.setClassName(mAppId, svcClass);
    HiddenAPIs.INS.fireSvcIntent(intent, mAppId, svcClass, 0, fg);
  }

  private final Object LOG_LOCK = new Object();
  private ILogCallback mLogCallback;
  private SingleTaskExecutor mLogExecutor;

  boolean isDebug() {
    return mLogCallback != null;
  }

  boolean setDebug(IBinder logCallback) {
    synchronized (LOG_LOCK) {
      boolean wasDebug = isDebug();
      mLogCallback = logCallback == null ? null : ILogCallback.Stub.asInterface(logCallback);

      if (wasDebug && !isDebug()) {
        mLogExecutor.cancel(true);
        mLogExecutor = null;
      } else if (!wasDebug && isDebug()) {
        mLogExecutor = LogUtil.readLogcat(this::writeToLogFile);
        if (mLogExecutor == null) {
          mLogCallback = null;
          return false;
        }
      }

      return true;
    }
  }

  private boolean writeToLogFile(String line) {
    synchronized (LOG_LOCK) {
      if (mLogCallback != null) {
        try {
          if (mLogCallback.writeToLogFile(line)) {
            return isDebug();
          }
        } catch (RemoteException e) {
          DaemonLog.e(TAG, "writeToLogFile", e);
          mLogCallback = null;
        }
      }

      if (mLogExecutor != null) {
        mLogExecutor.cancel(true);
        mLogExecutor = null;
      }

      return false;
    }
  }
}
