package com.mirfatif.privtasks;

import android.os.Build;
import android.os.Build.VERSION;
import android.util.Log;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Locale;

public class Util {

  private Util() {}

  public static String getDeviceInfo() {
    return "Android "
        + VERSION.SDK_INT
        + "\nBuild type: "
        + Build.TYPE
        + "\nDevice: "
        + Build.DEVICE
        + "\nManufacturer: "
        + Build.MANUFACTURER
        + "\nModel: "
        + Build.MODEL
        + "\nProduct: "
        + Build.PRODUCT;
  }

  public static String getCurrDateTime() {
    return new SimpleDateFormat("dd-MMM-yy_HH-mm-ss", Locale.ENGLISH)
        .format(System.currentTimeMillis());
  }

  public static final String LOG_FILE_PREFIX = "PMX_";
  public static final String LOG_FILE_DAEMON_PREFIX = "PMXD_";
  public static final String LOG_FILE_SUFFIX = ".log";

  public static File getCrashLogFile(String logDir, boolean isDaemon) {
    String prefix = isDaemon ? LOG_FILE_DAEMON_PREFIX : LOG_FILE_PREFIX;
    return new File(logDir, prefix + getCurrDateTime() + LOG_FILE_SUFFIX);
  }

  public static void debugLog(String tag, String message) {
    Log.d(tag, message + " - " + System.nanoTime());
  }
}
