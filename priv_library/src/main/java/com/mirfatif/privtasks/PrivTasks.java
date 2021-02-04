package com.mirfatif.privtasks;

import android.content.pm.PackageManager;
import android.content.pm.PermissionGroupInfo;
import android.content.pm.PermissionInfo;
import android.os.Build;
import android.os.Process;
import android.util.Log;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.Callback;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIsError;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIsException;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIsImpl;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PrivTasks {

  private static final String TAG = "PrivTaks";
  private final boolean DEBUG; // Verbose logging

  private final HiddenAPIs mHiddenAPIs;

  public PrivTasks(boolean isDebug) {
    DEBUG = isDebug;
    mHiddenAPIs = new HiddenAPIsImpl();
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// APP OPS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private int NUM_OP = -1;

  public Integer getNumOps() throws HiddenAPIsError {
    if (NUM_OP == -1) {
      NUM_OP = mHiddenAPIs.getNumOps();
    }
    return NUM_OP;
  }

  public List<Integer> buildOpToDefaultModeList() throws HiddenAPIsError {
    Integer opNum = getNumOps();
    if (opNum == null) {
      return null;
    }
    List<Integer> opToDefModeList = new ArrayList<>();
    for (int i = 0; i < opNum; i++) {
      opToDefModeList.add(mHiddenAPIs.opToDefaultMode(i));
    }
    return opToDefModeList;
  }

  public List<Integer> buildOpToSwitchList() throws HiddenAPIsError {
    Integer opNum = getNumOps();
    if (opNum == null) {
      return null;
    }
    List<Integer> opToSwitchList = new ArrayList<>();
    for (int i = 0; i < opNum; i++) {
      opToSwitchList.add(mHiddenAPIs.opToSwitch(i));
    }
    return opToSwitchList;
  }

  public List<String> buildOpToNameList() throws HiddenAPIsError {
    Integer opNum = getNumOps();
    if (opNum == null) {
      return null;
    }
    List<String> appOpsList = new ArrayList<>();
    for (int i = 0; i < opNum; i++) {
      appOpsList.add(mHiddenAPIs.opToName(i));
    }
    return appOpsList;
  }

  public List<String> buildModeToNameList() throws HiddenAPIsError {
    List<String> appOpsModes = new ArrayList<>();
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      for (int i = 0; i < mHiddenAPIs.getOpModeNamesSize(); i++) {
        appOpsModes.add(mHiddenAPIs.modeToName(i));
      }
    } else {
      appOpsModes = Arrays.asList("allow", "ignore", "deny", "default");
    }
    return appOpsModes;
  }

  public List<String> buildPermToOpCodeList(PackageManager pm) throws HiddenAPIsError {
    List<String> permToOpCodeList = new ArrayList<>();
    List<String> permGroupsList = new ArrayList<>();
    if (pm != null) {
      for (PermissionGroupInfo pgi : pm.getAllPermissionGroups(0)) {
        permGroupsList.add(pgi.name);
      }
    } else {
      try {
        for (Object pgi : mHiddenAPIs.getPermGroupInfoList()) {
          permGroupsList.add(((PermissionGroupInfo) pgi).name);
        }
      } catch (HiddenAPIsException e) {
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
          rateLimitLog("buildPermToOpCodeList: " + e.toString(), false);
        }
      } else {
        try {
          for (Object object : mHiddenAPIs.getPermInfoList(permGroup)) {
            permInfoList.add((PermissionInfo) object);
          }
        } catch (HiddenAPIsException e) {
          e.printStackTrace();
        }
      }
    }

    for (PermissionInfo permInfo : permInfoList) {
      if (!permInfo.packageName.equals("android")) {
        continue;
      }
      int opCode = mHiddenAPIs.permissionToOpCode(permInfo.name);
      if (opCode != mHiddenAPIs.getOpNone()) {
        permToOpCodeList.add(permInfo.name + ":" + opCode);
      }
    }
    return permToOpCodeList;
  }

  public Integer setAppOpsMode(String[] args) throws HiddenAPIsError {
    if (haveWrongArgs(args, 4)) {
      return null;
    }
    int op = mHiddenAPIs.strDebugOpToOp(args[1]);
    int uid = Integer.parseInt(args[2]);
    String pkgName = args[3];
    int mode = Integer.parseInt(args[4]);

    try {
      if (pkgName.equals("null")) {
        mHiddenAPIs.setUidMode(op, uid, mode);
      } else {
        mHiddenAPIs.setMode(op, uid, pkgName, mode);
      }
      return null;
    } catch (HiddenAPIsException e) {
      e.printStackTrace();
      return -1;
    }
  }

  public Integer resetAppOps(String[] args) {
    if (haveWrongArgs(args, 2)) {
      return null;
    }
    try {
      mHiddenAPIs.resetAllModes(Integer.parseInt(args[1]), args[2]);
      return null;
    } catch (HiddenAPIsException e) {
      e.printStackTrace();
      return -1;
    }
  }

  public List<MyPackageOps> getOpsForPackage(String[] args) {
    if (haveWrongArgs(args, 3)) {
      return null;
    }
    return getMyPackageOpsList(Integer.parseInt(args[1]), args[2], args[3]);
  }

  public List<MyPackageOps> getMyPackageOpsList(int uid, String packageName, String op)
      throws HiddenAPIsError {
    try {
      return mHiddenAPIs.getMyPackageOpsList(
          uid, packageName, op, getNumOps(), new PkgOpsListCallback());
    } catch (HiddenAPIsException e) {
      rateLimitThrowable(e);
      return null;
    }
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////// MANIFEST PERMISSIONS //////////////////////
  //////////////////////////////////////////////////////////////////

  public Integer getPermissionFlags(String[] args) {
    if (haveWrongArgs(args, 3)) {
      return null;
    }
    try {
      return mHiddenAPIs.getPermissionFlags(args[1], args[2], Integer.parseInt(args[3]));
    } catch (HiddenAPIsException e) {
      rateLimitThrowable(e);
      return null;
    }
  }

  public Integer grantRevokePermission(boolean grant, String[] args) {
    if (haveWrongArgs(args, 3)) {
      return null;
    }
    try {
      if (grant) {
        mHiddenAPIs.grantRuntimePermission(args[1], args[2], Integer.parseInt(args[3]));
      } else {
        mHiddenAPIs.revokeRuntimePermission(args[1], args[2], Integer.parseInt(args[3]));
      }
      return null;
    } catch (HiddenAPIsException e) {
      e.printStackTrace();
      return -1;
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PACKAGES ////////////////////////////
  //////////////////////////////////////////////////////////////////

  public Integer setAppEnabledState(boolean enabled, String[] args) {
    if (haveWrongArgs(args, 2)) {
      return null;
    }
    String pkg = args[1];
    int userId = Integer.parseInt(args[2]);
    String callingPkg = "shell:" + Process.myUid();

    int state;
    if (enabled) {
      state = PackageManager.COMPONENT_ENABLED_STATE_ENABLED;
    } else {
      state = PackageManager.COMPONENT_ENABLED_STATE_DISABLED_USER;
    }

    try {
      mHiddenAPIs.setApplicationEnabledSetting(pkg, state, 0, userId, callingPkg);
      return null;
    } catch (HiddenAPIsException e) {
      e.printStackTrace();
      return -1;
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PROCESSES ///////////////////////////
  //////////////////////////////////////////////////////////////////

  public int[] getPidsForCommands(String[] commands) {
    return mHiddenAPIs.getPidsForCommands(commands);
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////// COMMON METHODS /////////////////////////
  //////////////////////////////////////////////////////////////////

  public boolean canUseIAppOpsService() {
    return mHiddenAPIs.canUseIAppOpsService();
  }

  public boolean canUseIPm() {
    return mHiddenAPIs.canUseIPm();
  }

  private boolean haveWrongArgs(String[] cmd, int count) {
    if (cmd.length == count + 1) {
      return false;
    }
    Log.e(TAG, "Bad command: " + Arrays.toString(cmd));
    return true;
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

  private void rateLimitLog(String msg, boolean isError) {
    if (DEBUG) {
      if (isError) {
        Log.e(TAG, msg + " - " + System.nanoTime());
      } else {
        Log.i(TAG, msg + " - " + System.nanoTime());
      }
    } else if (System.currentTimeMillis() - lastLogTimestamp >= 1000) {
      if (isError) {
        Log.e(TAG, msg);
      } else {
        Log.i(TAG, msg);
      }
      lastLogTimestamp = System.currentTimeMillis();
    }
  }

  private class PkgOpsListCallback implements Callback {

    @Override
    public void onGetUidOpsNpException(Exception e) {
      // Hey Android! You are buggy.
      if (DEBUG) {
        e.printStackTrace();
      }
    }

    @Override
    public void onInvalidOpCode(int opCode, String pkgName) {
      rateLimitLog("getMyPackageOpsList: bad op: " + opCode + " for package: " + pkgName, true);
    }
  }
}
