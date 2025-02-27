package com.mirfatif.permissionmanagerx.util;

import android.os.Build;
import androidx.lifecycle.Lifecycle.State;
import androidx.lifecycle.LifecycleOwner;
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

  public static String getAndroidVersionInt() {
    return switch (Build.VERSION.SDK_INT) {
      case 24 -> "7";
      case 25 -> "7.1";
      case 26 -> "8";
      case 27 -> "8.1";
      case 28 -> "9";
      case 29 -> "10";
      case 30 -> "11";
      case 31 -> "12";
      case 32 -> "12.1";
      case 33 -> "13";
      case 34 -> "14";
      default -> "???";
    };
  }
}
