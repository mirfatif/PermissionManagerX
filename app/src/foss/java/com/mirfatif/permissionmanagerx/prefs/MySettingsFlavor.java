package com.mirfatif.permissionmanagerx.prefs;

import android.content.Context;
import android.content.SharedPreferences;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.Permission;

public enum MySettingsFlavor {
  INS;

  public Boolean isPkgInstallDate() {
    return null;
  }

  public boolean allowCriticalChanges() {
    return false;
  }

  public Boolean handleSearchQuery(String queryText, Package pkg, Permission permission) {
    return null;
  }

  public SharedPreferences getNoBackupPrefs() {
    return App.getCxt()
        .getSharedPreferences(
            App.getCxt().getPackageName() + "_no_backup_prefs", Context.MODE_PRIVATE);
  }
}
