package com.mirfatif.privdaemon;

import android.content.pm.PackageManager;
import android.os.Process;
import android.os.RemoteException;
import com.mirfatif.privtasks.AppPrivTasks;
import com.mirfatif.privtasks.bind.PrivsStatus;
import com.mirfatif.privtasks.bind.PrivsStatus.PermStatus;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;

public class PrivsStatusReader {

  private PrivsStatusReader() {}

  static PrivsStatus getStatus(AppPrivTasks appPrivTasks) throws RemoteException {
    PrivsStatus status = new PrivsStatus();
    for (String perm : PERMISSIONS) {
      int s = HiddenAPIs.INS.checkPermission(perm, Process.myPid(), Process.myUid());
      status.permStatusList.add(new PermStatus(perm, s == PackageManager.PERMISSION_GRANTED));
    }
    return appPrivTasks.setAppOpsStatus(status);
  }

  private static final String[] PERMISSIONS =
      new String[] {
        "android.permission.CREATE_USERS",
        "android.permission.MANAGE_USERS",
        "android.permission.INTERACT_ACROSS_USERS",
        "android.permission.INTERACT_ACROSS_USERS_FULL",
        "android.permission.GET_APP_OPS_STATS",
        "android.permission.MANAGE_APP_OPS_MODES",
        "android.permission.GRANT_RUNTIME_PERMISSIONS",
        "android.permission.REVOKE_RUNTIME_PERMISSIONS",
        "android.permission.OBSERVE_GRANT_REVOKE_PERMISSIONS",
        "android.permission.CHANGE_COMPONENT_ENABLED_STATE",
        "android.permission.PACKAGE_USAGE_STATS",
        "android.permission.REAL_GET_TASKS",
        "android.permission.GET_TASKS",
        "android.permission.SET_ACTIVITY_WATCHER"
      };
}
