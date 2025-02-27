package com.mirfatif.permissionmanagerx.prefs;

import com.mirfatif.permissionmanagerx.parser.PackageParser;

public class BackupRestoreFlavor {

  private BackupRestoreFlavor() {}

  public static void onRestoreDone() {
    PackageParser.INS.updatePkgList();
  }
}
