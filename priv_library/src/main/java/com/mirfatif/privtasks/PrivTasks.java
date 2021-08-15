package com.mirfatif.privtasks;

import android.app.AppOpsManager;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PermissionGroupInfo;
import android.content.pm.PermissionInfo;
import android.os.Build;
import android.os.Process;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenAPIsCallback;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIsError;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIsException;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIsImpl;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PrivTasks {

  private static final String TAG = "PrivTaks";

  private final HiddenAPIsCallbackImpl mHiddenAPIsCallbackImpl = new HiddenAPIsCallbackImpl();
  private final HiddenAPIs mHiddenAPIs = HiddenAPIsImpl.getInstance(mHiddenAPIsCallbackImpl);
  private final PrivTasksCallback mCallback;
  private final String mAppId;
  private final int mAppUserId;
  private final boolean mIsDaemon;

  public PrivTasks(PrivTasksCallback callback, String appId, int appUserId, boolean isDaemon) {
    mCallback = callback;
    mAppId = appId;
    mAppUserId = appUserId;
    mIsDaemon = isDaemon;
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

  // LOS N:
  // https://github.com/LineageOS/android_frameworks_base/blob/cm-14.1/core/java/android/app/AppOpsManager.java#L1350
  private boolean mUseOpToDefLOS = false;

  public List<Integer> buildOpToDefaultModeList() throws HiddenAPIsError {
    Integer opNum = getNumOps();
    if (opNum == null) {
      return null;
    }
    List<Integer> opToDefModeList = new ArrayList<>();
    boolean failed = false;
    for (int i = 0; i < opNum; i++) {
      if (failed) {
        opToDefModeList.add(HiddenAPIs.getStaticIntField("MODE_DEFAULT", AppOpsManager.class));
        continue;
      }
      try {
        opToDefModeList.add(mHiddenAPIs.opToDefaultMode(i, mUseOpToDefLOS));
      } catch (HiddenAPIsException e) {
        if (e.getCause() instanceof ArrayIndexOutOfBoundsException) {
          // OEM you are shit!
          failed = true;
          opToDefModeWorking = false;
          sendRequest(Commands.OP_NUM_INCONSISTENCY);
          e.printStackTrace();
        }
      } catch (HiddenAPIsError e) {
        if (e.getCause() instanceof NoSuchMethodError && mIsDaemon) {
          if (mUseOpToDefLOS) {
            // OEM you are shit!
            failed = true;
            sendRequest(Commands.OP_TO_DEF_MODE_NOT_FOUND);
          } else {
            mUseOpToDefLOS = true;
            return buildOpToDefaultModeList();
          }
          e.printStackTrace();
        } else {
          throw e;
        }
      }
    }
    return opToDefModeList;
  }

  public List<Integer> buildOpToSwitchList() throws HiddenAPIsError {
    Integer opNum = getNumOps();
    if (opNum == null) {
      return null;
    }
    List<Integer> opToSwitchList = new ArrayList<>();
    boolean failed = false;
    for (int i = 0; i < opNum; i++) {
      if (failed) {
        opToSwitchList.add(i);
        continue;
      }
      try {
        opToSwitchList.add(mHiddenAPIs.opToSwitch(i));
      } catch (HiddenAPIsException e) {
        if (e.getCause() instanceof ArrayIndexOutOfBoundsException) {
          // OEM you are shit!
          failed = true;
          opToSwitchWorking = false;
          sendRequest(Commands.OP_NUM_INCONSISTENCY);
          e.printStackTrace();
        }
      }
    }
    return opToSwitchList;
  }

  public List<String> buildOpToNameList() throws HiddenAPIsError {
    Integer opNum = getNumOps();
    if (opNum == null) {
      return null;
    }
    List<String> appOpsList = new ArrayList<>();
    boolean failed = false;
    for (int i = 0; i < opNum; i++) {
      if (failed) {
        appOpsList.add("UNKNOWN");
        continue;
      }
      try {
        appOpsList.add(mHiddenAPIs.opToName(i));
      } catch (HiddenAPIsException e) {
        if (e.getCause() instanceof ArrayIndexOutOfBoundsException) {
          // OEM you are shit!
          failed = true;
          opToNameWorking = false;
          sendRequest(Commands.OP_NUM_INCONSISTENCY);
          e.printStackTrace();
        }
      }
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
      appOpsModes =
          Arrays.asList(
              Commands.APP_OP_MODE_ALLOW,
              Commands.APP_OP_MODE_IGNORE,
              Commands.APP_OP_MODE_DENY,
              Commands.APP_OP_MODE_DEFAULT);
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
        sendRequest(Commands.GET_PERM_GRP_INFO_LIST_FAILED);
        e.printStackTrace();
      }
    }
    permGroupsList.add(null);

    List<PermissionInfo> permInfoList = new ArrayList<>();
    for (String permGroup : permGroupsList) {
      if (pm != null) {
        try {
          permInfoList = pm.queryPermissionsByGroup(permGroup, 0);
        } catch (NameNotFoundException e) {
          mCallback.logE(TAG + ": buildPermToOpCodeList: " + e.toString());
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

  public void setAppOpsMode(String[] args) throws HiddenAPIsError {
    if (!haveWrongArgs(args, 4)) {
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
      } catch (HiddenAPIsException e) {
        sendRequest(Commands.SET_APP_OPS_MODE_FAILED);
        e.printStackTrace();
      }
    }
  }

  public void resetAppOps(String[] args) {
    if (!haveWrongArgs(args, 2)) {
      try {
        mHiddenAPIs.resetAllModes(Integer.parseInt(args[1]), args[2]);
      } catch (HiddenAPIsException e) {
        sendRequest(Commands.RESET_APP_OPS_FAILED);
        e.printStackTrace();
      }
    }
  }

  public List<MyPackageOps> getOpsForPackage(String[] args) {
    if (haveWrongArgs(args, 3, true)) {
      return null;
    }
    return getMyPackageOpsList(Integer.parseInt(args[1]), args[2], args[3]);
  }

  public List<MyPackageOps> getMyPackageOpsList(int uid, String packageName, String op)
      throws HiddenAPIsError {
    try {
      return mHiddenAPIs.getMyPackageOpsList(uid, packageName, op, getNumOps());
    } catch (HiddenAPIsException e) {
      rateLimitThrowable(e);
      return null;
    }
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////// MANIFEST PERMISSIONS //////////////////////
  //////////////////////////////////////////////////////////////////

  public Integer getPermissionFlags(String[] args) {
    if (haveWrongArgs(args, 3, true)) {
      return null;
    }
    try {
      return mHiddenAPIs.getPermissionFlags(args[1], args[2], Integer.parseInt(args[3]));
    } catch (HiddenAPIsException e) {
      rateLimitThrowable(e);
      return null;
    }
  }

  public void grantRevokePermission(boolean grant, String[] args) {
    if (!haveWrongArgs(args, 3)) {
      try {
        if (grant) {
          mHiddenAPIs.grantRuntimePermission(args[1], args[2], Integer.parseInt(args[3]));
        } else {
          mHiddenAPIs.revokeRuntimePermission(args[1], args[2], Integer.parseInt(args[3]));
        }
      } catch (HiddenAPIsException e) {
        sendRequest(grant ? Commands.GRANT_PERM_FAILED : Commands.REVOKE_PERM_FAILED);
        e.printStackTrace();
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PACKAGES ////////////////////////////
  //////////////////////////////////////////////////////////////////

  public void setAppEnabledState(boolean enable, String[] args) {
    if (!haveWrongArgs(args, 2)) {
      String pkg = args[1];
      int userId = Integer.parseInt(args[2]);
      String callingPkg = "shell:" + Process.myUid();

      int state;
      if (enable) {
        state = PackageManager.COMPONENT_ENABLED_STATE_ENABLED;
      } else {
        state = PackageManager.COMPONENT_ENABLED_STATE_DISABLED_USER;
      }

      try {
        mHiddenAPIs.setApplicationEnabledSetting(pkg, state, 0, userId, callingPkg);
      } catch (HiddenAPIsException e) {
        sendRequest(enable ? Commands.ENABLE_PKG_FAILED : Commands.DISABLE_PKG_FAILED);
        e.printStackTrace();
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////////////// OTHERS ////////////////////////////
  //////////////////////////////////////////////////////////////////

  public int[] getPidsForCommands(String[] commands) {
    return mHiddenAPIs.getPidsForCommands(commands);
  }

  public void openAppInfo(String[] args) {
    if (!haveWrongArgs(args, 2)) {
      String pkgName = args[1];
      int userId = Integer.parseInt(args[2]);

      try {
        int res = mHiddenAPIs.openAppInfo(pkgName, userId);
        if (res != getAmSuccessCode()) {
          mCallback.logE(TAG + ": openAppInfo: result code: " + res);
        }
      } catch (HiddenAPIsException e) {
        sendRequest(Commands.OPEN_APP_INFO_FAILED);
        e.printStackTrace();
      }
    }
  }

  private int getAmSuccessCode() {
    try {
      return mHiddenAPIs.getAmSuccessCode();
    } catch (HiddenAPIsError e) {
      mCallback.logE(TAG + ": getAmSuccessCode: " + e.toString());
      return 0;
    }
  }

  // Must be called only when the app is in foreground or a fg service is running.
  public void sendRequest(String command, String codeWord) {
    try {
      mHiddenAPIs.sendRequest(command, mAppId, mAppUserId, codeWord);
    } catch (HiddenAPIsException e) {
      e.printStackTrace();
    }
  }

  public List<PermStatus> getPermStatus() {
    try {
      List<PermStatus> permStatusList = new ArrayList<>();
      for (String perm : PERMISSIONS) {
        int status = mHiddenAPIs.checkPermission(perm, Process.myPid(), Process.myUid());
        permStatusList.add(new PermStatus(perm, status == PackageManager.PERMISSION_GRANTED));
      }
      return permStatusList;
    } catch (HiddenAPIsException e) {
      e.printStackTrace();
      return null;
    }
  }

  private final String[] PERMISSIONS =
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

  private boolean opToDefModeWorking = true;
  private boolean opToSwitchWorking = true;
  private boolean opToNameWorking = true;
  private boolean opNumConsistent = true;

  public Integer getAppOpsStatus() {
    int status = 0;
    if (opToDefModeWorking) {
      status |= Commands.OP_TO_DEF_MODE_WORKS;
    }
    if (opToSwitchWorking) {
      status |= Commands.OP_TO_SWITCH_WORKS;
    }
    if (opToNameWorking) {
      status |= Commands.OP_TO_NAME_WORKS;
    }
    if (opNumConsistent) {
      status |= Commands.OP_NUM_CONSISTENT;
    }
    return status;
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

  @SuppressWarnings("BooleanMethodIsAlwaysInverted")
  boolean haveWrongArgs(String[] cmd, int count) {
    return haveWrongArgs(cmd, count, false);
  }

  private boolean haveWrongArgs(String[] cmd, int count, boolean rateLimitToast) {
    if (cmd.length == count + 1) {
      return false;
    }
    if (rateLimitToast) {
      rateLimitSendRequest(Commands.WRONG_ARGS_RECEIVED);
    } else {
      sendRequest(Commands.WRONG_ARGS_RECEIVED);
    }
    mCallback.logE(TAG + ": Bad command: " + Arrays.toString(cmd));
    return true;
  }

  private long lastThrowableTimestamp = 0;

  private void rateLimitThrowable(Throwable t) {
    if (!mCallback.isDebug() && System.currentTimeMillis() - lastThrowableTimestamp < 1000) {
      return;
    }
    t.printStackTrace();
    lastThrowableTimestamp = System.currentTimeMillis();
  }

  private long lastLogTimestamp = 0;

  private void rateLimitLog(String msg) {
    if (mCallback.isDebug()) {
      mCallback.logE(TAG + ": " + msg + " - " + System.nanoTime());
    } else if (System.currentTimeMillis() - lastLogTimestamp >= 1000) {
      mCallback.logE(TAG + ": " + msg);
      lastLogTimestamp = System.currentTimeMillis();
    }
  }

  private long lastRequestTimestamp = 0;

  private void rateLimitSendRequest(String cmd) {
    if (System.currentTimeMillis() - lastRequestTimestamp >= 60000) {
      sendRequest(cmd);
      lastRequestTimestamp = System.currentTimeMillis();
    }
  }

  public void sendRequest(String command) {
    mCallback.sendRequest(command);
  }

  @SuppressWarnings("UnusedDeclaration")
  public void logError(String msg) {
    mCallback.logE(TAG + ": " + msg);
  }

  private class HiddenAPIsCallbackImpl implements HiddenAPIsCallback {

    @Override
    public void onGetUidOpsNpException(Exception e) {
      // Hey Android! You are buggy.
      if (mCallback.isDebug()) {
        mCallback.logE(TAG + ": getMyPackageOpsList: " + e.toString());
      } else {
        rateLimitLog("getMyPackageOpsList: " + e.toString());
      }
    }

    @Override
    public void onInvalidOpCode(int opCode, String pkgName) {
      // OEM you are shit!
      opNumConsistent = false;
      rateLimitSendRequest(Commands.OP_NUM_INCONSISTENCY);
      rateLimitLog("getMyPackageOpsList: bad op: " + opCode + " for package: " + pkgName);
    }
  }

  public interface PrivTasksCallback {

    boolean isDebug();

    void logE(String msg);

    void sendRequest(String command);
  }
}
