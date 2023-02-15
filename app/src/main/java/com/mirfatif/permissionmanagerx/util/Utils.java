package com.mirfatif.permissionmanagerx.util;

import androidx.lifecycle.Lifecycle.State;
import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.privtasks.util.MyLog;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class Utils {

  private Utils() {}

  public static boolean isAlive(LifecycleOwner lifecycleOwner) {
    return lifecycleOwner.getLifecycle().getCurrentState().isAtLeast(State.INITIALIZED);
  }

  public static boolean copyStream(String tag, InputStream input, OutputStream output) {
    byte[] buffer = new byte[8192];
    int len;

    try {
      while ((len = input.read(buffer)) != -1) {
        output.write(buffer, 0, len);
      }
      return true;
    } catch (IOException e) {
      MyLog.e(tag, "copyStream", e);
      return false;
    }
  }

  public static boolean isPsProVersion() {
    return BuildConfig.VERSION_NAME.contains("-ps-pro");
  }

  public static boolean isSelfProVersion() {
    return BuildConfig.VERSION_NAME.contains("-pro") && !isPsProVersion();
  }

  public static boolean isFreeVersion() {
    return !isPsProVersion() && !isSelfProVersion();
  }

  public static boolean isAmazonVersion() {
    return BuildConfig.VERSION_NAME.contains("-amaz");
  }

  public static boolean isFdroidVersion() {
    return BuildConfig.VERSION_NAME.contains("-fd");
  }
}
