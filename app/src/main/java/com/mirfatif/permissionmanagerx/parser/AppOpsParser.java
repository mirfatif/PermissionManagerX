package com.mirfatif.permissionmanagerx.parser;

import android.os.RemoteException;
import android.util.ArrayMap;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermsDb;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.privtasks.AppPrivTasks;
import com.mirfatif.privtasks.bind.AppOpsLists;
import com.mirfatif.privtasks.bind.MyPackageOps;
import com.mirfatif.privtasks.util.CloseableReadWriteLock;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public enum AppOpsParser {
  INS;

  private static final String TAG = "AppOpsParser";

  List<MyPackageOps> getOpsForPackage(int uid, String pkgName, Integer op) {
    int[] ops = op == null ? null : new int[] {op};

    if (DaemonHandler.INS.isDaemonAlive()) {
      return DaemonIface.INS.getOpsForPkg(uid, pkgName, ops);
    }

    if (canReadAppOpsNoDaemon()) {
      try {
        return mAppPrivTasks.getOpsForPkg(uid, pkgName, ops);
      } catch (RemoteException e) {
        handleException("getOpsForPackage", e);
        return null;
      }
    }

    return new ArrayList<>();
  }

  List<MyPackageOps> getUidOps(int uid) {
    return getOpsForPackage(uid, null, null);
  }

  private final List<String> mAppOpsNames = Collections.synchronizedList(new ArrayList<>());
  private final List<String> mAppOpsModes = Collections.synchronizedList(new ArrayList<>());
  private final List<Integer> mOpSwitchList = Collections.synchronizedList(new ArrayList<>());
  private final List<Integer> mOpDefModeList = Collections.synchronizedList(new ArrayList<>());
  private final Map<String, Integer> mPermToOpCodeMap =
      Collections.synchronizedMap(new ArrayMap<>());

  private final CloseableReadWriteLock mListsLock = new CloseableReadWriteLock();

  private @interface ListsStatus {
    int NOT_BUILT = 0;
    int BUILT_IN_APP = 1;
    int BUILT_WITH_DAEMON = 2;
  }

  private int mListsStatus = ListsStatus.NOT_BUILT;

  public void buildAppOpsList() {
    mListsLock.withWriteLock(this::buildAppOpsListInternal);
  }

  private void buildAppOpsListInternal() {
    if (mListsStatus == ListsStatus.BUILT_WITH_DAEMON) {
      return;
    }

    AppOpsLists appOpsLists;

    if (DaemonHandler.INS.isDaemonAlive()) {
      appOpsLists = DaemonIface.INS.getAppOpsLists();
      if (appOpsLists == null) {
        mWorksWithDaemon = false;
        return;
      }

      mWorksWithDaemon = true;

      if (mListsStatus == ListsStatus.BUILT_IN_APP) {
        mHasAppOps = false;
        mAppOpsNames.clear();
        mAppOpsModes.clear();
        mOpSwitchList.clear();
        mOpDefModeList.clear();
        mPermToOpCodeMap.clear();
      }
      mListsStatus = ListsStatus.BUILT_WITH_DAEMON;
    } else if (mListsStatus == ListsStatus.BUILT_IN_APP) {
      return;
    } else if (canReadAppOpsNoDaemon()) {
      try {
        appOpsLists = mAppPrivTasks.getAppOpsLists(App.getPm());
        mListsStatus = ListsStatus.BUILT_IN_APP;
      } catch (RemoteException e) {
        handleException("buildAppOpsList", e);
        return;
      }
    } else {
      return;
    }

    mAppOpsNames.addAll(appOpsLists.appOpsNames);
    mAppOpsModes.addAll(appOpsLists.appOpsModes);
    mOpSwitchList.addAll(appOpsLists.opSwitchList);
    mOpDefModeList.addAll(appOpsLists.opDefModeList);
    mPermToOpCodeMap.putAll(appOpsLists.permToOpMap.map);

    mHasAppOps = true;

    BgRunner.execute(() -> ExcFiltersData.INS.populateExtraAppOpsList(true, false));

    if (MySettings.INS.shouldFixPermDb() && fixPermDb()) {
      PermsDb.INS.buildRefs();
    }
  }

  public boolean fixPermDb() {
    if (mListsStatus != ListsStatus.BUILT_WITH_DAEMON) {
      return false;
    }

    List<PermissionEntity> entities = PermsDb.INS.getDb().getAll();

    mListsLock.withReadLock(
        () -> entities.removeIf(e -> e.isAppOps || !mAppOpsNames.contains(e.permName)));
    entities.forEach(entity -> entity.isAppOps = true);

    PermsDb.INS.getDb().insertAll(entities.toArray(new PermissionEntity[0]));

    MyLog.i(
        TAG,
        "fixPermDb",
        "Fixed 'isAppOps' field in " + entities.size() + " permission references");

    MySettings.INS.setFixPermDb(false);
    return true;
  }

  public List<String> getAppOpsNames() {
    return mListsLock.withReadLock(() -> new ArrayList<>(mAppOpsNames));
  }

  public String getAppOpName(int op) {
    return mListsLock.withReadLock(
        () -> op >= 0 && op < mAppOpsNames.size() ? mAppOpsNames.get(op) : null);
  }

  public Integer getAppOpCode(String opName) {
    int i = mListsLock.withReadLock(() -> mAppOpsNames.indexOf(opName));
    return i >= 0 ? i : null;
  }

  public int getAppOpModeCount() {
    return mListsLock.withReadLock(mAppOpsModes::size);
  }

  public boolean isValidAppOpMode(int opMode) {
    return mListsLock.withReadLock(() -> opMode >= 0 && opMode < mAppOpsModes.size());
  }

  public String opModeToName(int opMode) {
    return mListsLock.withReadLock(
        () -> isValidAppOpMode(opMode) ? mAppOpsModes.get(opMode) : null);
  }

  public String getDependsOn(int op) {
    return mListsLock.withReadLock(
        () -> {
          Integer opSwitch = op >= 0 && op < mOpSwitchList.size() ? mOpSwitchList.get(op) : null;
          return opSwitch == null
                  || opSwitch < 0
                  || opSwitch >= mAppOpsNames.size()
                  || op == opSwitch
              ? null
              : mAppOpsNames.get(opSwitch);
        });
  }

  Integer getOpDefMode(int op) {
    return mListsLock.withReadLock(
        () -> op >= 0 && op < mOpDefModeList.size() ? mOpDefModeList.get(op) : null);
  }

  Integer getPermToOpCode(String perm) {
    return mListsLock.withReadLock(() -> mPermToOpCodeMap.get(perm));
  }

  private boolean mWorksWithNoDaemon = true;
  private boolean mWorksWithDaemon = true;
  private boolean mHasAppOps = false;

  private boolean canReadAppOpsNoDaemon() {
    return ApiUtils.hasAppOpsPerm() && mWorksWithNoDaemon;
  }

  public boolean canReadAppOps() {
    return (DaemonHandler.INS.isDaemonAlive() && mWorksWithDaemon) || canReadAppOpsNoDaemon();
  }

  public boolean hasAppOps() {
    return mHasAppOps && !MySettings.INS.excludeAppOpsPerms() && canReadAppOps();
  }

  private void handleException(String method, Throwable t) {
    mWorksWithNoDaemon = false;
    MyLog.e(TAG, method, t.toString());
  }

  public final AppPrivTasks mAppPrivTasks = new AppPrivTasks(new AppPrivTasksCallbackImpl(), false);

  private static class AppPrivTasksCallbackImpl implements AppPrivTasks.AppPrivTasksCallback {

    public void logErr(String tag, String method, Throwable e) {
      MyLog.e(tag, method, e);
    }

    public void logErr(String tag, String method, String err) {
      MyLog.e(tag, method, err);
    }

    public void showError(int error) {
      DaemonHandler.INS.showError(error);
    }
  }
}
