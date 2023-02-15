package com.mirfatif.privdaemon;

import com.mirfatif.privtasks.util.MyLog;

public class DaemonLog {

  private DaemonLog() {}

  private static String makeTag(String tag) {
    return PrivDaemon.TAG + (tag == null ? "" : ": " + tag);
  }

  public static void e(String tag, String method, Throwable e) {
    MyLog.e(makeTag(tag), method, e);
  }

  public static void e(String tag, String method, String err) {
    MyLog.e(makeTag(tag), method, err);
  }

  public static void w(String tag, String method, String msg) {
    MyLog.w(makeTag(tag), method, msg);
  }

  public static void i(String tag, String method, String msg) {
    MyLog.i(makeTag(tag), method, msg);
  }

  public static void d(String tag, String method, String msg) {
    if (Callbacks.INS.isDebug()) {
      MyLog.d(makeTag(tag), method, msg);
    }
  }
}
