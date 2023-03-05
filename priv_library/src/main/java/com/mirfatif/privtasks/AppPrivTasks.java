package com.mirfatif.privtasks;

import android.app.AppOpsManager;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PermissionGroupInfo;
import android.content.pm.PermissionInfo;
import android.os.Build;
import android.os.RemoteException;
import com.mirfatif.err.HiddenAPIsException;
import com.mirfatif.privtasks.bind.AppOpsLists;
import com.mirfatif.privtasks.bind.MyPackageOps;
import com.mirfatif.privtasks.bind.PrivsStatus;
import com.mirfatif.privtasks.bind.StrIntMap;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;
import com.mirfatif.privtasks.util.bg.RateLimitedTask;
import com.mirfatif.privtasks.util.bg.RateLimitedTaskTyped;
import com.mirfatif.privtasks.util.bg.RateLimiter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class AppPrivTasks {

  private static final String TAG = "AppPrivTasks";

  private final AppPrivTasksCallback mCallback;
  private final boolean mDaemon;

  public AppPrivTasks(AppPrivTasksCallback callback, boolean isDaemon) {
    mCallback = callback;
    mDaemon = isDaemon;
  }

  private boolean opToDefModeWorks = true;
  private boolean opToSwitchWorks = true;
  private boolean opToNameWorks = true;
  private boolean getOpsWorks = true;
  private boolean opNumConsistent = true;
  private boolean opModeConsistent = true;

  public PrivsStatus setAppOpsStatus(PrivsStatus status) {
    status.opToDefModeWorks = opToDefModeWorks;
    status.opToSwitchWorks = opToSwitchWorks;
    status.opToNameWorks = opToNameWorks;
    status.getOpsWorks = getOpsWorks;
    status.opNumConsistent = opNumConsistent;
    status.opModeConsistent = opModeConsistent;
    return status;
  }

  public AppOpsLists getAppOpsLists(PackageManager pm) throws RemoteException {
    return new AppOpsLists(
        buildAppOpsNames(),
        buildAppOpsModes(),
        buildOpDefModeList(),
        buildOpSwitchList(),
        buildPermToOpMap(pm));
  }

  private List<String> buildAppOpsNames() throws HiddenAPIsException {
    List<String> appOpsNames = new ArrayList<>();
    boolean failed = false;
    for (int i = 0; i < HiddenSdkConstants._NUM_OP.get(!mDaemon); i++) {
      if (failed) {
        appOpsNames.add(Constants.UNKNOWN_OP);
        continue;
      }
      try {
        String opName = HiddenAPIs.opToName(i);
        if ("NONE".equals(opName)
            || "deprecated".equals(opName)
            || (opName.startsWith("Unknown(") && opName.endsWith(")"))) {
          opName = Constants.UNKNOWN_OP;
        }
        appOpsNames.add(opName);
      } catch (ArrayIndexOutOfBoundsException e) {

        failed = true;
        opToNameWorks = opNumConsistent = false;
        mAppOpsErrorCb.mOpNumErrSender.run();
        mCallback.logErr(TAG, "buildAppOpsNames", e);
      } catch (NoSuchMethodError e) {
        if (mDaemon) {
          throw e;
        } else {
          throw new HiddenAPIsException(e);
        }
      }
    }
    return appOpsNames;
  }

  private List<String> buildAppOpsModes() throws HiddenAPIsException {
    List<String> modeNames =
        new ArrayList<>(
            Arrays.asList(
                Constants.APP_OP_MODE_ALLOW,
                Constants.APP_OP_MODE_IGNORE,
                Constants.APP_OP_MODE_DENY,
                Constants.APP_OP_MODE_DEFAULT));

    List<String> appOpsModes = new ArrayList<>();

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      String mode;
      for (int i = 0; i < HiddenSdkConstants.MODE_NAMES_SIZE.get(!mDaemon); i++) {
        try {
          mode = HiddenAPIs.modeToName(i);
          mode = Character.toUpperCase(mode.charAt(0)) + mode.substring(1);
          appOpsModes.add(mode);
        } catch (NoSuchMethodError e) {
          if (mDaemon) {
            throw e;
          } else {
            throw new HiddenAPIsException(e);
          }
        }
      }
    } else {
      appOpsModes = new ArrayList<>(modeNames);
    }

    List<Integer> modes =
        new ArrayList<>(
            Arrays.asList(
                AppOpsManager.MODE_ALLOWED,
                AppOpsManager.MODE_IGNORED,
                AppOpsManager.MODE_ERRORED,
                AppOpsManager.MODE_DEFAULT));

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
      modes.add(AppOpsManager.MODE_FOREGROUND);
      modeNames.add(Constants.APP_OP_MODE_FG);
    }

    for (int i = 0; i < modes.size(); i++) {
      int mode = modes.get(i);
      String modeName = modeNames.get(i);
      if (appOpsModes.indexOf(modeName) != mode) {
        opModeConsistent = false;
        mCallback.showError(PrivTasksError.OP_MODE_INCONSISTENCY);
        throw new HiddenAPIsException("Bad AppOp modes");
      }
    }

    return appOpsModes;
  }

  private boolean mUseOpToDefLOS = false;

  private List<Integer> buildOpDefModeList() throws HiddenAPIsException {
    List<Integer> opToDefModeList = new ArrayList<>();
    boolean failed = false;
    for (int i = 0; i < HiddenSdkConstants._NUM_OP.get(!mDaemon); i++) {
      if (failed) {
        opToDefModeList.add(AppOpsManager.MODE_DEFAULT);
        continue;
      }
      try {
        opToDefModeList.add(HiddenAPIs.opToDefaultMode(i, mUseOpToDefLOS));
      } catch (ArrayIndexOutOfBoundsException e) {

        failed = true;
        opToDefModeWorks = opNumConsistent = false;
        mAppOpsErrorCb.mOpNumErrSender.run();
        mCallback.logErr(TAG, "buildOpDefModeList", e);
      } catch (NoSuchMethodError e) {
        if (mUseOpToDefLOS) {

          if (mDaemon) {
            throw e;
          } else {
            throw new HiddenAPIsException(e);
          }
        } else {
          mCallback.logErr(TAG, "buildOpDefModeList", e);
          mUseOpToDefLOS = true;
          return buildOpDefModeList();
        }
      }
    }

    return opToDefModeList;
  }

  private List<Integer> buildOpSwitchList() throws HiddenAPIsException {
    List<Integer> opToSwitchList = new ArrayList<>();
    boolean failed = false;
    for (int i = 0; i < HiddenSdkConstants._NUM_OP.get(!mDaemon); i++) {
      if (failed) {
        opToSwitchList.add(i);
        continue;
      }
      try {
        opToSwitchList.add(HiddenAPIs.opToSwitch(i));
      } catch (ArrayIndexOutOfBoundsException e) {

        failed = true;
        opToSwitchWorks = opNumConsistent = false;
        mAppOpsErrorCb.mOpNumErrSender.run();
        mCallback.logErr(TAG, "buildOpSwitchList", e);
      } catch (NoSuchMethodError e) {
        if (mDaemon) {
          throw e;
        } else {
          throw new HiddenAPIsException(e);
        }
      }
    }
    return opToSwitchList;
  }

  private boolean mUsePkgMgr = false;

  private StrIntMap buildPermToOpMap(PackageManager pm) throws RemoteException {
    List<String> permGroupsList = new ArrayList<>();

    if (mUsePkgMgr && pm != null) {
      pm.getAllPermissionGroups(0).forEach(pgi -> permGroupsList.add(pgi.name));
    } else {
      try {
        HiddenAPIs.INS
            .getPermGroupInfoList()
            .forEach(pgi -> permGroupsList.add(((PermissionGroupInfo) pgi).name));
      } catch (RemoteException e) {
        if (pm == null) {
          throw e;
        } else {
          mUsePkgMgr = true;
          return buildPermToOpMap(pm);
        }
      } catch (NoSuchMethodError e) {
        if (mDaemon) {
          throw e;
        } else {
          throw new HiddenAPIsException(e);
        }
      }
    }
    permGroupsList.add(null);

    List<PermissionInfo> permInfoList = new ArrayList<>();

    for (String permGroup : permGroupsList) {
      if (mUsePkgMgr && pm != null) {
        try {
          permInfoList.addAll(pm.queryPermissionsByGroup(permGroup, 0));
        } catch (NameNotFoundException e) {

          mCallback.logErr(TAG, "buildPermToOpMap", e.toString());
        }
      } else {
        try {
          HiddenAPIs.INS
              .getPermInfoList(permGroup)
              .forEach(pi -> permInfoList.add((PermissionInfo) pi));
        } catch (RemoteException e) {
          if (pm == null) {
            throw e;
          } else {
            mUsePkgMgr = true;
            return buildPermToOpMap(pm);
          }
        } catch (NoSuchMethodError e) {
          if (mDaemon) {
            throw e;
          } else {
            throw new HiddenAPIsException(e);
          }
        }
      }
    }

    final StrIntMap permToOpMap = new StrIntMap();
    final int OP_NONE = HiddenSdkConstants.OP_NONE.get(!mDaemon);

    for (PermissionInfo permInfo : permInfoList) {

      if (!permInfo.packageName.equals("android")) {
        continue;
      }

      try {
        int opCode = HiddenAPIs.permToOpCode(permInfo.name);
        if (opCode != OP_NONE) {
          permToOpMap.map.put(permInfo.name, opCode);
        }
      } catch (NoSuchMethodError e) {
        if (mDaemon) {
          throw e;
        } else {
          throw new HiddenAPIsException(e);
        }
      }
    }

    return permToOpMap;
  }

  public List<MyPackageOps> getOpsForPkg(int uid, String pkgName, int[] ops)
      throws RemoteException {
    try {
      return HiddenAPIs.INS.getOpsForPkg(uid, pkgName, ops, mAppOpsErrorCb);
    } catch (NoSuchMethodError | SecurityException e) {
      if (mDaemon) {
        throw e;
      } else {
        throw new HiddenAPIsException(e);
      }
    }
  }

  private final AppOpsErrorCallbackImpl mAppOpsErrorCb = new AppOpsErrorCallbackImpl();

  private class AppOpsErrorCallbackImpl implements HiddenAPIs.AppOpsErrorCallback {

    private final RateLimitedTask mAppOpsImplErrSender =
        new RateLimitedTask(
            2, TimeUnit.MINUTES, () -> mCallback.showError(PrivTasksError.APP_OPS_IMPL));

    private final RateLimitedTask mOpNumErrSender =
        new RateLimitedTask(
            2, TimeUnit.MINUTES, () -> mCallback.showError(PrivTasksError.OP_NUM_INCONSISTENCY));

    private final RateLimitedTaskTyped<Exception> mNPELogger =
        new RateLimitedTaskTyped<>(
            1, TimeUnit.SECONDS, e -> mCallback.logErr(TAG, "getOpsForPkg", e));

    private final RateLimiter mOpCodeLogLimiter = new RateLimiter(1, TimeUnit.SECONDS);

    public void onGetUidOpsNpException(Exception e) {

      getOpsWorks = false;
      mNPELogger.run(e);
      mAppOpsImplErrSender.run();
    }

    public void onInvalidOpCode(int opCode, String pkgName) {

      opNumConsistent = false;
      if (mOpCodeLogLimiter.can(true)) {
        mCallback.logErr(TAG, "getOpsForPkg", "Bad op: " + opCode + " for package: " + pkgName);
      }
      mOpNumErrSender.run();
    }
  }

  public interface AppPrivTasksCallback {

    void logErr(String tag, String method, Throwable e);

    void logErr(String tag, String method, String err);

    void showError(int error);
  }
}
