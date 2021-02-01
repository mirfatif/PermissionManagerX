package com.mirfatif.permissionmanagerx.prefs;

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
}
