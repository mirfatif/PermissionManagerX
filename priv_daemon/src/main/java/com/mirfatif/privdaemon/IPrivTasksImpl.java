package com.mirfatif.privdaemon;

import android.Manifest;
import android.app.AppOpsManager;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Debug;
import android.os.IBinder;
import android.os.Process;
import android.os.RemoteException;
import com.mirfatif.err.HiddenAPIsException;
import com.mirfatif.privtasks.AppPrivTasks;
import com.mirfatif.privtasks.Constants;
import com.mirfatif.privtasks.HiddenSdkConstants;
import com.mirfatif.privtasks.bind.AppOpsLists;
import com.mirfatif.privtasks.bind.DaemonState;
import com.mirfatif.privtasks.bind.IPrivTasksCallback;
import com.mirfatif.privtasks.bind.MyPackageOps;
import com.mirfatif.privtasks.bind.PermFixedFlags;
import com.mirfatif.privtasks.bind.PrivsStatus;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;
import com.mirfatif.privtasks.iface.IPrivTasks;
import java.io.File;
import java.io.IOException;
import java.util.List;

public class IPrivTasksImpl extends IPrivTasks.Stub {

  private final IPrivTasksFlavorImpl mIPrivTasksFlavor;
  private final AppPrivTasks mAppPrivTasks;

  IPrivTasksImpl() {
    AppPrivTasks.AppPrivTasksCallback cb = new AppPrivTasksCallbackImpl();
    mAppPrivTasks = new AppPrivTasks(cb, true);
    mIPrivTasksFlavor = new IPrivTasksFlavorImpl(cb);
  }

  public void sendStdErr(int port, String jniLibPath) throws RemoteException {
    Jni.INS.loadLib(jniLibPath);
    Callbacks.INS.sendStdErr(port);
  }

  public void hello(IBinder privTasksCb, String crashLogFile) throws RemoteException {
    IPrivTasksCallback callback = IPrivTasksCallback.Stub.asInterface(privTasksCb);
    Callbacks.INS.update(callback, crashLogFile);
    callback.hello(
        new DaemonState(
            Process.myPid(),
            Process.myUid(),
            Callbacks.getSEContext(),
            Server.INS.getPort(),
            mIPrivTasksFlavor.asBinder()));
  }

  public void setExitOnAppDeath(boolean exitOnAppDeath) {
    Callbacks.INS.setExitOnAppDeath(exitOnAppDeath);
  }

  public PrivsStatus getPrivsStatus() throws RemoteException {
    return PrivsStatusReader.getStatus(mAppPrivTasks);
  }

  public boolean setDebug(IBinder logCb) {
    return Callbacks.INS.setDebug(logCb);
  }

  public void dumpHeap(String directory) throws RemoteException {
    System.gc();

    File dir = new File(directory);
    if (!dir.isDirectory()) {
      throw new HiddenAPIsException(directory + " is not a directory");
    }

    File file = new File(dir, "com.mirfatif.privdaemon.pmx.hprof");
    try {

      Debug.dumpHprofData(file.getAbsolutePath());
    } catch (IOException e) {
      throw new HiddenAPIsException(e);
    }
  }

  public void grantAppPrivileges(String pkgName, int uid) throws RemoteException {

    setAppOpMode(
        uid, pkgName, HiddenSdkConstants.OP_RUN_IN_BACKGROUND.get(), AppOpsManager.MODE_ALLOWED);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      setAppOpMode(
          uid,
          pkgName,
          HiddenSdkConstants.OP_RUN_ANY_IN_BACKGROUND.get(),
          AppOpsManager.MODE_ALLOWED);
    }

    HiddenAPIs.INS.addPowerSaveWhitelistApp(pkgName);

    HiddenAPIs.INS.grantRuntimePermission(pkgName, Constants.PERM_GET_APP_OPS_STATS, 0);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
      HiddenAPIs.INS.grantRuntimePermission(pkgName, Manifest.permission.POST_NOTIFICATIONS, 0);
    }
  }

  public void stopDaemon() {
    mIPrivTasksFlavor.onDaemonStopped();
    Callbacks.INS.exit();
  }

  public AppOpsLists getAppOpsLists() throws RemoteException {
    return mAppPrivTasks.getAppOpsLists(null);
  }

  public PermFixedFlags getPermFixedFlags() throws RemoteException {
    return HiddenSdkConstants.getPermFixedFlags();
  }

  public int getPkgCountForUid(int uid) throws RemoteException {
    return HiddenAPIs.INS.getPackagesForUid(uid).length;
  }

  public List<MyPackageOps> getOpsForPackage(int uid, String pkgName, int[] ops)
      throws RemoteException {
    return mAppPrivTasks.getOpsForPkg(uid, pkgName, ops);
  }

  public int getPermFlags(String permName, String pkgName, int userId) throws RemoteException {
    return HiddenAPIs.INS.getPermFlags(permName, pkgName, userId);
  }

  public void setPermState(boolean grant, String pkgName, String permName, int userId)
      throws RemoteException {
    if (grant) {
      HiddenAPIs.INS.grantRuntimePermission(pkgName, permName, userId);
    } else {
      HiddenAPIs.INS.revokeRuntimePermission(pkgName, permName, userId);
    }
  }

  public void setAppOpMode(int uid, String pkgName, int op, int mode) throws RemoteException {
    if (pkgName == null) {
      HiddenAPIs.INS.setAppOpUidMode(uid, op, mode);
    } else {
      HiddenAPIs.INS.setAppOpMode(pkgName, uid, op, mode);
    }
  }

  public void resetAppOps(int userId, String pkgName) throws RemoteException {
    HiddenAPIs.INS.resetAllModes(userId, pkgName);
  }

  public void setPkgState(boolean enable, String pkgName, int userId) throws RemoteException {
    String callingPkg = "shell:" + Process.myUid();

    int state;
    if (enable) {
      state = PackageManager.COMPONENT_ENABLED_STATE_ENABLED;
    } else {
      state = PackageManager.COMPONENT_ENABLED_STATE_DISABLED_USER;
    }

    HiddenAPIs.INS.setApplicationEnabledSetting(pkgName, state, 0, userId, callingPkg);
  }

  public void openAppInfo(String pkgName, int userId) throws RemoteException {
    int res = HiddenAPIs.INS.openAppInfo(pkgName, userId);
    if (res != HiddenSdkConstants.START_SUCCESS.get()) {
      throw new HiddenAPIsException("Failed to open app info. Result code: " + res);
    }
  }

  private static class AppPrivTasksCallbackImpl implements AppPrivTasks.AppPrivTasksCallback {

    public void logErr(String tag, String method, Throwable e) {
      DaemonLog.e(tag, method, e);
    }

    public void logErr(String tag, String method, String err) {
      DaemonLog.e(tag, method, err);
    }

    public void showError(int error) {
      Callbacks.INS.showError(error);
    }
  }
}
