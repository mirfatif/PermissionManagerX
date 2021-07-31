package com.mirfatif.permissionmanagerx.prefs;

import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.Permission;

public class MySettingsFlavor {

  private static MySettingsFlavor mMySettingsFlavor;

  public static synchronized MySettingsFlavor getInstance() {
    if (mMySettingsFlavor == null) {
      mMySettingsFlavor = new MySettingsFlavor();
    }
    return mMySettingsFlavor;
  }

  private MySettingsFlavor() {}

  public Boolean isPkgInstallDate() {
    return null;
  }

  public boolean allowCriticalChanges() {
    return false;
  }

  @SuppressWarnings("UnusedDeclaration")
  public Boolean handleSearchQuery(String queryText, Package pkg, Permission permission) {
    return null;
  }
}
