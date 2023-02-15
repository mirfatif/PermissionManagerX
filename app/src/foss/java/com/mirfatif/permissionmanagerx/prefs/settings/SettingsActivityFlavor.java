package com.mirfatif.permissionmanagerx.prefs.settings;

import android.content.Intent;

public class SettingsActivityFlavor {

  public SettingsActivityFlavor(SettingsActivity activity) {}

  public void onCreate() {}

  static boolean shouldCloseOnBackPressed(Intent intent) {
    return false;
  }
}
