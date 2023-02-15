package com.mirfatif.privtasks.hiddenapis;

import static com.mirfatif.privtasks.hiddenapis.HiddenAPIsConstants.PKG_URI;
import static com.mirfatif.privtasks.hiddenapis.HiddenAPIsConstants.START_SVC_ERR;
import static com.mirfatif.privtasks.hiddenapis.HiddenAPIsConstants.getString;

import android.app.AppOpsManager;
import android.app.AppOpsManager.OpEntry;
import android.app.AppOpsManager.PackageOps;
import android.content.ComponentName;
import android.content.Intent;
import android.content.pm.ParceledListSlice;
import android.net.Uri;
import android.os.Build;
import android.os.Process;
import android.os.RemoteException;
import android.provider.Settings;
import com.mirfatif.err.HiddenAPIsException;
import com.mirfatif.privtasks.HiddenSdkConstants;
import com.mirfatif.privtasks.bind.MyPackageOps;
import java.util.ArrayList;
import java.util.List;

public enum HiddenAPIs {
  INS;

  public static int getNumOps() {
    return AppOpsManager.getNumOps();
  }

  public static int getOpModeNamesSize() {
    return AppOpsManager.MODE_NAMES.length;
  }

  public static int opToDefaultMode(int opCode, boolean isLos) {
    if (isLos) {
      return AppOpsManager.opToDefaultMode(opCode, false);
    } else {
      return AppOpsManager.opToDefaultMode(opCode);
    }
  }

  public static int opToSwitch(int opCode) {
    return AppOpsManager.opToSwitch(opCode);
  }

  public static String opToName(int opCode) {
    return AppOpsManager.opToName(opCode);
  }

  public static String modeToName(int opMode) {
    return AppOpsManager.modeToName(opMode);
  }

  public static int permToOpCode(String permName) {
    return AppOpsManager.permissionToOpCode(permName);
  }

  public static int strDebugOpToOp(String opName) {
    return AppOpsManager.strDebugOpToOp(opName);
  }

  public void setAppOpMode(String pkgName, int uid, int op, int mode) throws RemoteException {

    SysSvcFactory.INS.getIAppOpsSvc().setMode(op, uid, pkgName, mode);
  }

  public void setAppOpUidMode(int uid, int op, int mode) throws RemoteException {
    SysSvcFactory.INS.getIAppOpsSvc().setUidMode(op, uid, mode);
  }

  public void resetAllModes(int userId, String pkgName) throws RemoteException {
    SysSvcFactory.INS.getIAppOpsSvc().resetAllModes(userId, pkgName);
  }

  public List<MyPackageOps> getOpsForPkg(int uid, String pkgName, int[] ops, AppOpsErrorCallback cb)
      throws RemoteException {
    List<PackageOps> pkgOpsList = null;

    if (pkgName != null) {
      pkgOpsList = SysSvcFactory.INS.getIAppOpsSvc().getOpsForPackage(uid, pkgName, ops);
    } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      try {
        pkgOpsList = SysSvcFactory.INS.getIAppOpsSvc().getUidOps(uid, ops);
      } catch (NullPointerException e) {
        cb.onGetUidOpsNpException(e);
        return null;
      }
    }

    List<MyPackageOps> myPkgOpsList = new ArrayList<>();

    if (pkgOpsList == null) {
      return myPkgOpsList;
    }

    for (PackageOps pkgOps : pkgOpsList) {
      List<MyPackageOps.MyOpEntry> myOpEntryList = new ArrayList<>();

      for (OpEntry opEntry : pkgOps.getOps()) {
        MyPackageOps.MyOpEntry myOpEntry = new MyPackageOps.MyOpEntry();

        myOpEntry.op = opEntry.getOp();

        if (myOpEntry.op >= HiddenSdkConstants._NUM_OP.get()) {
          cb.onInvalidOpCode(myOpEntry.op, pkgOps.getPackageName());
          continue;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
          myOpEntry.lastAccessTime =
              opEntry.getLastAccessTime(HiddenSdkConstants.OP_FLAGS_ALL.get());
        } else {
          myOpEntry.lastAccessTime = opEntry.getTime();
        }
        myOpEntry.opMode = opEntry.getMode();

        myOpEntryList.add(myOpEntry);
      }

      myPkgOpsList.add(new MyPackageOps(pkgOps.getPackageName(), myOpEntryList));
    }

    return myPkgOpsList;
  }

  public interface AppOpsErrorCallback {

    void onGetUidOpsNpException(Exception e);

    void onInvalidOpCode(int opCode, String pkgName);
  }

  public List<?> getPermGroupInfoList() throws RemoteException {
    ParceledListSlice<?> pls;
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
      pls = SysSvcFactory.INS.getIPermMgr().getAllPermissionGroups(0);
    } else {
      pls = (ParceledListSlice<?>) SysSvcFactory.INS.getIPkgMgr().getAllPermissionGroups(0);
    }

    return pls != null ? pls.getList() : new ArrayList<>();
  }

  public List<?> getPermInfoList(String permGroup) throws RemoteException {
    ParceledListSlice<?> pls;
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
      pls = SysSvcFactory.INS.getIPermMgr().queryPermissionsByGroup(permGroup, 0);
    } else {
      pls =
          (ParceledListSlice<?>)
              SysSvcFactory.INS.getIPkgMgr().queryPermissionsByGroup(permGroup, 0);
    }
    return pls != null ? pls.getList() : new ArrayList<>();
  }

  public int getPermFlags(String permName, String pkgName, int userId) throws RemoteException {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      return SysSvcFactory.INS.getIPermMgr().getPermissionFlags(pkgName, permName, userId);
    } else if (Build.VERSION.SDK_INT == Build.VERSION_CODES.R) {
      return SysSvcFactory.INS.getIPermMgr().getPermissionFlags(permName, pkgName, userId);
    } else {
      return SysSvcFactory.INS.getIPkgMgr().getPermissionFlags(permName, pkgName, userId);
    }
  }

  public void grantRuntimePermission(String pkgName, String permName, int userId)
      throws RemoteException {
    SysSvcFactory.INS.getIPkgMgr().grantRuntimePermission(pkgName, permName, userId);
  }

  public void revokeRuntimePermission(String pkgName, String permName, int userId)
      throws RemoteException {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
      SysSvcFactory.INS.getIPermMgr().revokeRuntimePermission(pkgName, permName, userId, null);
    } else {
      SysSvcFactory.INS.getIPkgMgr().revokeRuntimePermission(pkgName, permName, userId);
    }
  }

  public int checkPermission(String perm, int pid, int uid) throws RemoteException {
    return SysSvcFactory.INS.getIActMgr().checkPermission(perm, pid, uid);
  }

  public void setApplicationEnabledSetting(
      String pkg, int state, int flags, int userId, String callingPkg) throws RemoteException {
    try {
      SysSvcFactory.INS
          .getIPkgMgr()
          .setApplicationEnabledSetting(pkg, state, flags, userId, callingPkg);
    } catch (IllegalArgumentException e) {

      throw new HiddenAPIsException(e);
    }
  }

  public String[] getPackagesForUid(int uid) throws RemoteException {
    return SysSvcFactory.INS.getIPkgMgr().getPackagesForUid(uid);
  }

  public void addPowerSaveWhitelistApp(String pkgName) throws RemoteException {
    SysSvcFactory.INS.getIDevIdleController().addPowerSaveWhitelistApp(pkgName);
  }

  public int openAppInfo(String pkgName, int userId) throws RemoteException {
    Intent intent =
        new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
            .setData(Uri.parse(getString(PKG_URI, pkgName)))
            .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
    return SysSvcFactory.INS
        .getIActMgr()
        .startActivityAsUser(null, null, intent, null, null, null, 0, 0, null, null, userId);
  }

  public void fireSvcIntent(Intent intent, String appId, String svcClass, int userId, boolean fg)
      throws RemoteException {
    ComponentName cn;
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
      cn = SysSvcFactory.INS.getIActMgr().startService(null, intent, null, fg, appId, null, userId);
    } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      cn = SysSvcFactory.INS.getIActMgr().startService(null, intent, null, fg, appId, userId);
    } else {
      cn = SysSvcFactory.INS.getIActMgr().startService(null, intent, null, appId, userId);
    }

    if (cn == null || !cn.getPackageName().equals(appId)) {
      throw new HiddenAPIsException(getString(START_SVC_ERR, svcClass));
    }
  }

  public static int[] getPidsForCommands(String[] commands) {
    return Process.getPidsForCommands(commands);
  }
}
