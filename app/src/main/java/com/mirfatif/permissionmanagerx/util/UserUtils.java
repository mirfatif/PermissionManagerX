package com.mirfatif.permissionmanagerx.util;

import android.os.Build;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;

public class UserUtils {

  private UserUtils() {}

  public static int getUserId(int uid) {
    return uid / 100000;
  }

  public static int getUserId() {
    return getUserId(android.os.Process.myUid());
  }

  public static String[] getPackagesForUid(int uid) {
    if (getUserId(uid) == 0 || Build.VERSION.SDK_INT < Build.VERSION_CODES.S) {
      return App.getPm().getPackagesForUid(uid);
    } else {
      return DaemonIface.INS.getPackagesForUid(uid);
    }
  }
}
