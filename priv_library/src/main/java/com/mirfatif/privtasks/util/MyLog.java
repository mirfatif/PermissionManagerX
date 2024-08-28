package com.mirfatif.privtasks.util;

import android.util.Log;

public class MyLog {

  private MyLog() {}

  private static String makeMsg(String method, String msg) {
    return (method == null ? "" : method + "(): ") + msg;
  }

  public static void e(String tag, String method, Throwable e) {
    e(tag, method, e.toString(), e);
  }

  public static void e(String tag, String method, String msg, Throwable e) {
    Log.e(tag, makeMsg(method, msg), e);
    e.printStackTrace(System.err);
  }

  public static void e(String tag, String method, String err) {
    err = makeMsg(method, err);
    Log.e(tag, err);
    System.err.println(tag + ": " + err);
  }

  public static void w(String tag, String method, String msg) {
    Log.w(tag, makeMsg(method, msg));
  }

  public static void i(String tag, String method, String msg) {
    Log.i(tag, makeMsg(method, msg));
  }

  public static void d(String tag, String method, String msg) {
    Log.d(tag, makeMsg(method, msg));
  }
}
