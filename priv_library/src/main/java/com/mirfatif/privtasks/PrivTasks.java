package com.mirfatif.privtasks;

import android.app.AppOpsManager;
import android.app.AppOpsManager.OpEntry;
import android.content.Context;
import android.content.pm.IPackageManager;
import android.content.pm.PackageManager;
import android.content.pm.ParceledListSlice;
import android.content.pm.PermissionGroupInfo;
import android.content.pm.PermissionInfo;
import android.os.Build;
import android.os.Process;
import android.os.RemoteException;
import android.os.ServiceManager;
import android.permission.IPermissionManager;
import android.util.Log;
import com.android.internal.app.IAppOpsService;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

// The following methods are moved from IPackageManager to IPermissionManager in SDK 30.
// getPermissionFlags, getAllPermissionGroups, queryPermissionsByGroup, revokeRuntimePermission
// So android.jar needs to be modified to build successfully, or use Reflection.

public class PrivTasks {

  private final boolean DEBUG; // Verbose logging

  private IAppOpsService mIAppOpsService;
  private IPackageManager mIPackageManager;
  private IPermissionManager mIPermissionManager;

  public PrivTasks(boolean isDebug, boolean initializeServices) throws NoSuchMethodError {
    DEBUG = isDebug;
    if (initializeServices) {
      initializeAppOpsService();
      initializePmService();
    }
  }

  // asInterface() and getService() hidden APIs
  public void initializeAppOpsService() throws NoSuchMethodError {
    mIAppOpsService =
        IAppOpsService.Stub.asInterface(ServiceManager.getService(Context.APP_OPS_SERVICE));
  }

  // asInterface() and getService() hidden APIs
  public void initializePmService() throws NoSuchMethodError {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
      // Context.PERMISSION_SERVICE doesn't work
      mIPermissionManager =
          IPermissionManager.Stub.asInterface(ServiceManager.getService("permissionmgr"));
    }
    mIPackageManager = IPackageManager.Stub.asInterface(ServiceManager.getService("package"));
  }

  public List<String> buildOpToNameList() {
    Integer opNum = getOpNum();
    if (opNum == null) {
      return null;
    }
    List<String> appOpsList = new ArrayList<>();
    for (int i = 0; i < opNum; i++) {
      // hidden API
      appOpsList.add(AppOpsManager.opToName(i));
    }
    return appOpsList;
  }

  public List<String> buildModeToNameList() {
    List<String> appOpsModes = new ArrayList<>();
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      // MODE_NAMES hidden API
      for (int i = 0; i < AppOpsManager.MODE_NAMES.length; i++) {
        // hidden API
        appOpsModes.add(AppOpsManager.modeToName(i));
      }
    } else {
      appOpsModes = Arrays.asList("allow", "ignore", "deny", "default");
    }
    return appOpsModes;
  }

  public List<MyPackageOps> getOpsForPackage(String[] args) {
    int uid = Integer.parseInt(args[1]);
    try {
      return getMyPackageOpsList(uid, args[2], args[3]);
    } catch (RemoteException e) {
      rateLimitThrowable(e);
      return null;
    }
  }

  public List<MyPackageOps> getMyPackageOpsList(int uid, String packageName, String op)
      throws RemoteException, NoSuchMethodError {
    int[] ops = op.equals("null") ? null : new int[] {Integer.parseInt(op)};
    List<AppOpsManager.PackageOps> pkgOpsList = null;

    if (!packageName.equals("null")) {
      // hidden API
      pkgOpsList = mIAppOpsService.getOpsForPackage(uid, packageName, ops);
    } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      try {
        // hidden API
        pkgOpsList = mIAppOpsService.getUidOps(uid, ops);
      } catch (NullPointerException e) {
        // Hey Android! You are buggy.
        if (DEBUG) {
          e.printStackTrace();
        }
        return new ArrayList<>();
      }
    }

    if (pkgOpsList == null) {
      return new ArrayList<>();
    }

    List<MyPackageOps> myPackageOpsList = new ArrayList<>();
    for (AppOpsManager.PackageOps packageOps : pkgOpsList) {
      MyPackageOps myPackageOps = new MyPackageOps();
      List<MyPackageOps.MyOpEntry> myOpEntryList = new ArrayList<>();

      for (AppOpsManager.OpEntry opEntry : packageOps.getOps()) {
        MyPackageOps.MyOpEntry myOpEntry = new MyPackageOps.MyOpEntry();

        // hidden API
        myOpEntry.op = opEntry.getOp();

        // MIUI returns 10005 op
        Integer opNum = getOpNum();
        if (opNum == null) {
          return null;
        }
        if (myOpEntry.op >= opNum) {
          rateLimitLog(
              "getMyPackageOpsList()",
              "Bad op: " + myOpEntry.op + " for package: " + packageOps.getPackageName(),
              true);
          continue;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
          // TODO crash
          //  NoSuchMethodError: No virtual method getLastAccessTime(I)J on SDK 28 / Realme RMX1851
          myOpEntry.lastAccessTime = opEntry.getLastAccessTime(AppOpsManager.OP_FLAGS_ALL);
        } else {
          // hidden API
          // N and O don't have getLastAccessTime()
          myOpEntry.lastAccessTime = getTime(opEntry);
        }
        myOpEntry.opMode = opEntry.getMode();

        myOpEntryList.add(myOpEntry);
      }

      myPackageOps.packageName = packageOps.getPackageName();
      myPackageOps.myOpEntryList = myOpEntryList;

      myPackageOpsList.add(myPackageOps);
    }
    return myPackageOpsList;
  }

  @SuppressWarnings("deprecation")
  private long getTime(OpEntry opEntry) {
    return opEntry.getTime();
  }

  public List<Integer> buildOpToDefaultModeList() {
    Integer opNum = getOpNum();
    if (opNum == null) {
      return null;
    }
    List<Integer> opToDefModeList = new ArrayList<>();
    for (int i = 0; i < opNum; i++) {
      // hidden API
      opToDefModeList.add(AppOpsManager.opToDefaultMode(i));
    }
    return opToDefModeList;
  }

  public List<Integer> buildOpToSwitchList() {
    Integer opNum = getOpNum();
    if (opNum == null) {
      return null;
    }
    List<Integer> opToSwitchList = new ArrayList<>();
    for (int i = 0; i < opNum; i++) {
      // hidden API
      opToSwitchList.add(AppOpsManager.opToSwitch(i));
    }
    return opToSwitchList;
  }

  private Integer NUM_OP = null;

  public Integer getOpNum() {
    if (NUM_OP == null) {
      try {
        // hidden API
        Field idField = AppOpsManager.class.getDeclaredField("_NUM_OP");
        NUM_OP = idField.getInt(idField);
      } catch (NoSuchFieldException | IllegalAccessException e) {
        rateLimitThrowable(e);
        return null;
      }
    }
    return NUM_OP;
  }

  public Integer getSystemFixedFlag() {
    try {
      // hidden API
      Field idField = PackageManager.class.getDeclaredField("FLAG_PERMISSION_SYSTEM_FIXED");
      return idField.getInt(idField);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      rateLimitThrowable(e);
      return null;
    }
  }

  public Integer getPermissionFlags(String[] args) {
    try {
      // hidden API
      /**
       * requires system permissions: {@link PackageManager#getPermissionFlags(String, String,
       * UserHandle)}
       */
      // TODO crash
      //  NumberFormatException: For input string: "TASKS" on SDK 29 / Xiaomi Redmi K20 Pro
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
        return mIPermissionManager.getPermissionFlags(args[1], args[2], Integer.parseInt(args[3]));
      } else {
        return mIPackageManager.getPermissionFlags(args[1], args[2], Integer.parseInt(args[3]));
      }
    } catch (SecurityException | RemoteException e) {
      rateLimitThrowable(e);
      return null;
    }
  }

  public List<String> buildPermToOpCodeList(PackageManager pm) {
    List<String> permToOpCodeList = new ArrayList<>();
    List<String> permGroupsList = new ArrayList<>();
    if (pm != null) {
      for (PermissionGroupInfo pgi : pm.getAllPermissionGroups(0)) {
        permGroupsList.add(pgi.name);
      }
    } else {
      try {
        // getAllPermissionGroups() hidden API
        List<?> pgiList = new ArrayList<>();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
          pgiList = mIPermissionManager.getAllPermissionGroups(0).getList();
        } else {
          ParceledListSlice<?> pls =
              (ParceledListSlice<?>) mIPackageManager.getAllPermissionGroups(0);
          if (pls != null) {
            pgiList = pls.getList();
          }
        }

        for (Object pgi : pgiList) {
          permGroupsList.add(((PermissionGroupInfo) pgi).name);
        }
      } catch (RemoteException e) {
        e.printStackTrace();
      }
    }
    permGroupsList.add(null);

    List<PermissionInfo> permInfoList = new ArrayList<>();
    for (String permGroup : permGroupsList) {
      if (pm != null) {
        try {
          permInfoList = pm.queryPermissionsByGroup(permGroup, 0);
        } catch (PackageManager.NameNotFoundException e) {
          rateLimitLog("buildPermToOpCodeList", e.toString(), false);
        }
      } else {
        try {
          // queryPermissionsByGroup() hidden API
          List<?> piList = new ArrayList<>();
          if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            piList = mIPermissionManager.queryPermissionsByGroup(permGroup, 0).getList();
          } else {
            ParceledListSlice<?> pls =
                (ParceledListSlice<?>) mIPackageManager.queryPermissionsByGroup(permGroup, 0);
            if (pls != null) {
              piList = pls.getList();
            }
          }

          for (Object object : piList) {
            permInfoList.add((PermissionInfo) object);
          }
        } catch (RemoteException e) {
          e.printStackTrace();
        }
      }
    }

    for (PermissionInfo permInfo : permInfoList) {
      if (!permInfo.packageName.equals("android")) {
        continue;
      }
      // hidden API
      int opCode = AppOpsManager.permissionToOpCode(permInfo.name);
      if (opCode != AppOpsManager.OP_NONE) {
        permToOpCodeList.add(permInfo.name + ":" + opCode);
      }
    }
    return permToOpCodeList;
  }

  public Integer grantRevokePermission(boolean grant, String[] args) {
    try {
      if (grant) {
        // hidden API
        // requires android.permission.GRANT_RUNTIME_PERMISSIONS
        mIPackageManager.grantRuntimePermission(args[1], args[2], Integer.parseInt(args[3]));
      } else {
        // hidden API
        // requires android.permission.REVOKE_RUNTIME_PERMISSIONS
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
          mIPermissionManager.revokeRuntimePermission(
              args[1], args[2], Integer.parseInt(args[3]), null);
        } else {
          mIPackageManager.revokeRuntimePermission(args[1], args[2], Integer.parseInt(args[3]));
        }
      }
      return null;
    } catch (RemoteException | SecurityException e) {
      // SecurityException is thrown if calling UID/PID doesn't have required permissions
      // e.g. ADB lacks these permissions on MIUI
      e.printStackTrace();
      return -1;
    }
  }

  public Integer setAppOpsMode(String[] args) {
    // hidden API
    int op = AppOpsManager.strDebugOpToOp(args[1]);
    int uid = Integer.parseInt(args[2]);
    String pkgName = args[3];
    int mode = Integer.parseInt(args[4]);

    try {
      if (pkgName.equals("null")) {
        mIAppOpsService.setUidMode(op, uid, mode);
      } else {
        // requires android.permission.UPDATE_APP_OPS_STATS
        mIAppOpsService.setMode(op, uid, pkgName, mode);
      }
      return null;
    } catch (RemoteException | SecurityException e) {
      e.printStackTrace();
      return -1;
    }
  }

  public Integer resetAppOps(String[] args) {
    try {
      // hidden API
      // requires android.permission.UPDATE_APP_OPS_STATS
      mIAppOpsService.resetAllModes(Integer.parseInt(args[1]), args[2]);
      return null;
    } catch (RemoteException | SecurityException e) {
      e.printStackTrace();
      return -1;
    }
  }

  public Integer setEnabledState(boolean enabled, String[] args) {
    String pkg = args[1];
    int userid = Integer.parseInt(args[2]);
    String callingPkg = "shell:" + Process.myUid();

    int state;
    if (enabled) {
      state = PackageManager.COMPONENT_ENABLED_STATE_ENABLED;
    } else {
      state = PackageManager.COMPONENT_ENABLED_STATE_DISABLED_USER;
    }

    try {
      // hidden API
      mIPackageManager.setApplicationEnabledSetting(pkg, state, 0, userid, callingPkg);
      return null;
    } catch (RemoteException e) {
      e.printStackTrace();
      return -1;
    }
  }

  private long lastThrowableTimestamp = 0;

  private void rateLimitThrowable(Throwable t) {
    if (!DEBUG && System.currentTimeMillis() - lastThrowableTimestamp < 1000) {
      return;
    }
    t.printStackTrace();
    lastThrowableTimestamp = System.currentTimeMillis();
  }

  private long lastLogTimestamp = 0;

  private void rateLimitLog(String tag, String msg, boolean isError) {
    if (DEBUG) {
      if (isError) {
        Log.e(tag, msg + " - " + System.nanoTime());
      } else {
        Log.i(tag, msg + " - " + System.nanoTime());
      }
    } else if (System.currentTimeMillis() - lastLogTimestamp >= 1000) {
      if (isError) {
        Log.e(tag, msg);
      } else {
        Log.i(tag, msg);
      }
      lastLogTimestamp = System.currentTimeMillis();
    }
  }
}
