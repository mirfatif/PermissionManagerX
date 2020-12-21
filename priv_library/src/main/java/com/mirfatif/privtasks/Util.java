package com.mirfatif.privtasks;

import android.util.Log;

public class Util {

  private Util() {}

  public static void debugLog(String tag, String message) {
    Log.d(tag, message + " - " + System.nanoTime());
  }
}
