package com.mirfatif.permissionmanagerx.prefs;

import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.Permission;

public enum MySettingsFlavor {
  INSTANCE;

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
