package com.mirfatif.permissionmanagerx.parser;

import static android.content.pm.PermissionInfo.PROTECTION_INTERNAL;

import android.app.AppOpsManager;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PermissionInfo;
import android.content.pm.Signature;
import android.os.Build;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.RemoteException;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleEventObserver;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.ProcessLifecycleOwner;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.PermGroupsMapping.PermGroupInfo;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermsDb;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.UserUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveEvent;
import com.mirfatif.permissionmanagerx.util.bg.UiRunner;
import com.mirfatif.privtasks.Constants;
import com.mirfatif.privtasks.HiddenSdkConstants;
import com.mirfatif.privtasks.bind.MyPackageOps;
import com.mirfatif.privtasks.bind.PermFixedFlags;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.Util;
import com.mirfatif.privtasks.util.bg.RateLimitedTaskTyped;
import com.mirfatif.privtasks.util.bg.SingleParamTask;
import com.mirfatif.privtasks.util.bg.SingleSchedTaskExecutor;
import com.mirfatif.privtasks.util.bg.SingleTaskExecutorTyped;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public enum PackageParser {
  INS;

  private static final String TAG = "PackageParser";

  private final PackageManager mPm = App.getPm();

  private final LiveEvent<List<Package>> mPkgListLive = new LiveEvent<>(false);
  private final LiveEvent<Package> mChangedPkg = new LiveEvent<>(true);
  private final LiveEvent<Integer> mProgMax = new LiveEvent<>(true);
  private final LiveEvent<Integer> mProgNow = new LiveEvent<>(true);
  private final LiveEvent<Boolean> mListInProgress = new LiveEvent<>(false);
  private final LiveEvent<Integer> mListCompleted = new LiveEvent<>(true);

  private final List<PackageInfo> mPkgInfoList = new ArrayList<>();
  private final List<Package> mPkgList = new ArrayList<>();

  public boolean mOpModesConsistent = true;

  private final SingleTaskExecutorTyped<List<Package>> mPkgUpdater =
      new SingleTaskExecutorTyped<>(
          this::updatePkgListInternal, null, TAG + "-PkgUpdater", Thread.MAX_PRIORITY);

  private final SingleSchedTaskExecutor mPkgInfoListCleaner =
      new SingleSchedTaskExecutor(this::clearPkgInfoList, TAG + "-PkgInfoListCleaner");

  public void updatePkgListIfChanged() {
    if (mPkgInfoList.isEmpty() || mPkgList.isEmpty()) {
      updatePkgList();
    }
  }

  public void updatePkgList() {
    updatePkgList(false);
  }

  public void updatePkgList(boolean rebuildPkgInfoList) {
    if (rebuildPkgInfoList) {
      clearPkgInfoList();
    }

    mPkgUpdater.cancelAndSubmit(true);
  }

  public List<Package> updatePkgListWithResult(boolean rebuildPkgInfoList) {
    if (rebuildPkgInfoList) {
      clearPkgInfoList();
    }
    return mPkgUpdater.cancelSubmitGet(true);
  }

  private List<Package> updatePkgListInternal() {
    mListInProgress.postValue(true);

    mPkgInfoListCleaner.cancel(false);
    List<PackageInfo> pkgInfoList = buildPkgInfoList();

    buildRequiredData();

    setProgress(pkgInfoList.size(), true, true);

    List<Package> pkgList = new ArrayList<>();

    int size = pkgInfoList.size();

    for (int i = 0; i < size; i++) {
      if (Thread.interrupted()) {
        return null;
      }

      setProgress(i, false, false);
      PackageInfo pkgInfo = pkgInfoList.get(i);

      if (pkgInfo == null) {

        continue;
      }

      Package pkg = new Package();
      if (isPkgUpdated(pkgInfo, pkg, true)) {
        pkgList.add(pkg);
        PkgParserFlavor.INS.onPkgCreated(pkg);

        if (mDoRepeatUpdates) {
          updateAndPostLivePkgList(pkgList, false);
        }
      }
    }

    setProgress(size, false, true);

    PkgParserFlavor.INS.sortPkgListAgain(pkgList);

    updateAndPostLivePkgList(pkgList, true);

    PkgParserFlavor.INS.onPkgListCompleted();

    mPkgInfoListCleaner.cancelAndSchedule(false, 30, TimeUnit.SECONDS);

    mListInProgress.postValue(false);

    return getPkgList();
  }

  private boolean isUpdating() {
    return mPkgUpdater.hasRunningOrPendingTasks();
  }

  private List<PackageInfo> buildPkgInfoList() {
    List<PackageInfo> pkgInfoList = new ArrayList<>(mPkgInfoList);

    if (!pkgInfoList.isEmpty()) {
      return pkgInfoList;
    }

    setProgress(CREATE_LIST, true, false);

    pkgInfoList.clear();
    pkgInfoList.addAll(PkgParserFlavor.INS.getPackageList());

    setProgress(SORT_LIST, true, false);
    PkgParserFlavor.INS.sortPkgList(pkgInfoList);

    pkgInfoList.removeIf(Objects::isNull);

    synchronized (mPkgInfoList) {
      mPkgInfoList.clear();
      mPkgInfoList.addAll(pkgInfoList);
    }

    return pkgInfoList;
  }

  {
    UiRunner.post(
        () ->
            ProcessLifecycleOwner.get()
                .getLifecycle()
                .addObserver(
                    (LifecycleEventObserver)
                        (source, event) -> {
                          if (event == Lifecycle.Event.ON_STOP) {
                            clearPkgInfoList();
                            setRepeatUpdates(true);
                          }
                        }));
  }

  public void clearPkgInfoList() {
    synchronized (mPkgInfoList) {
      mPkgInfoList.clear();
    }
  }

  private boolean mAppCanReadFlags = true;
  private Integer SYSTEM_FIXED_FLAG, POLICY_FIXED_FLAG;

  private static final Object BUILD_DATA_LOCK = new Object();

  public void buildRequiredData() {
    synchronized (BUILD_DATA_LOCK) {
      if (!PermsDb.INS.refsBuilt()) {
        setProgress(REF_PERMS_LIST, true, false);
        PermsDb.INS.buildRefs();
      }

      setProgress(APP_OPS_LISTS, true, false);
      AppOpsParser.INS.buildAppOpsList();

      PkgParserFlavor.INS.buildRequiredData();

      if (SYSTEM_FIXED_FLAG != null && POLICY_FIXED_FLAG != null) {
        return;
      }

      PermFixedFlags flags;

      if (DaemonHandler.INS.isDaemonAlive()) {
        flags = DaemonIface.INS.getPermFixedFlags();
        if (flags == null) {
          return;
        }
      } else if (mAppCanReadFlags) {
        try {
          flags = HiddenSdkConstants.getPermFixedFlags();
        } catch (RemoteException e) {
          mAppCanReadFlags = false;
          MyLog.e(TAG, "buildRequiredData", e.toString());
          return;
        }
      } else {
        return;
      }

      SYSTEM_FIXED_FLAG = flags.systemFixed;
      POLICY_FIXED_FLAG = flags.policyFixed;
    }
  }

  public Integer getSystemFixedFlag() {
    return SYSTEM_FIXED_FLAG;
  }

  public List<Package> getPkgList() {
    synchronized (mPkgList) {
      return new ArrayList<>(mPkgList);
    }
  }

  public Package getPkg(int position) {
    synchronized (mPkgList) {
      if (position < 0 || position >= mPkgList.size()) {
        MyLog.e(TAG, "getPkg", "Bad position: " + position);
        return null;
      }
      return mPkgList.get(position);
    }
  }

  public int getPkgPosition(Package pkg) {
    synchronized (mPkgList) {
      int position = mPkgList.indexOf(pkg);
      if (position == -1) {
        MyLog.e(TAG, "getPkgPosition", "Bad Package provided");
        return -1;
      }
      return position;
    }
  }

  public void updatePackage(Package pkg, boolean filterPerms) {
    PackageInfo packageInfo = PkgParserFlavor.INS.getPackageInfo(pkg);

    if (packageInfo == null || !isPkgUpdated(packageInfo, pkg, filterPerms)) {
      removePackage(pkg);
      return;
    }

    mChangedPkg.postValue(pkg, true);

    if (MySettings.INS.isSearching()) {
      updateSearchLists(pkg, true);
    }
  }

  public void removePackage(Package pkg) {
    if (isUpdating()) {
      updatePkgList();
      return;
    }
    boolean res;
    synchronized (mPkgList) {
      res = mPkgList.remove(pkg);
    }
    if (res) {
      if (MySettings.INS.isSearching()) {
        removeSearchPackage(pkg);
      } else {
        postLivePkgList(mPkgList, true);
      }
      pkg.setPkgRemoved(true);
    } else {
      MyLog.e(TAG, "removePackage", "Bad Package provided");
    }
  }

  public LiveData<List<Package>> getPkgListLive() {
    return mPkgListLive;
  }

  public LiveData<Package> getChangedPkg() {
    return mChangedPkg;
  }

  public LiveData<Integer> getProgMax() {
    return mProgMax;
  }

  public LiveData<Integer> getProgNow() {
    return mProgNow;
  }

  public LiveData<Integer> getListCompleted() {
    return mListCompleted;
  }

  private void updateAndPostLivePkgList(List<Package> pkgList, boolean isFinal) {
    synchronized (mPkgList) {
      if (pkgList != null) {
        mPkgList.clear();
        mPkgList.addAll(pkgList);
      }
    }

    if (!MySettings.INS.isSearching()) {
      postLivePkgList(mPkgList, isFinal);
      if (isFinal) {
        sendListCompleted(PostListStatus.FINAL, mPkgList.size());
      }
    } else {
      handleSearchQuery(isFinal ? PostListStatus.FINAL : PostListStatus.NOT_FINAL);
    }
  }

  private void postLivePkgList(List<Package> pkgList, boolean mustSend) {
    mPkgListLive.postValue(new ArrayList<>(pkgList), mustSend);
  }

  private boolean mDoRepeatUpdates = true;

  public void setRepeatUpdates(boolean doRepeatUpdates) {
    mDoRepeatUpdates = doRepeatUpdates;
  }

  private final RateLimitedTaskTyped<Integer> mProgPoster =
      new RateLimitedTaskTyped<>(100, TimeUnit.MILLISECONDS, mProgNow::postValue);

  private void setProgress(int value, boolean isMax, boolean isFinal) {
    if (isMax) {
      mProgMax.postValue(value, isFinal);
      PkgParserFlavor.INS.setProgress(true, value);
    } else {
      if (isFinal) {
        mProgNow.postValue(value, true);
      } else {
        mProgPoster.run(value);
      }
      PkgParserFlavor.INS.setProgress(false, value);
    }
  }

  private void sendListCompleted(int isFinal, int pkgCount) {
    if (isFinal == PostListStatus.FINAL || (isFinal == PostListStatus.UNDEFINED && !isUpdating())) {
      mListCompleted.postValue(pkgCount, true);
    }
  }

  public LiveData<Boolean> getListInProg() {
    return mListInProgress;
  }

  private static final int CREATE_LIST = -1;
  private static final int SORT_LIST = -2;
  private static final int REF_PERMS_LIST = -3;
  private static final int APP_OPS_LISTS = -4;

  public int getProgMsg(int progMax) {
    return switch (progMax) {
      case CREATE_LIST -> R.string.creating_packages_list;
      case SORT_LIST -> R.string.sorting_packages_list;
      case REF_PERMS_LIST -> R.string.reading_reference_perms;
      case APP_OPS_LISTS -> R.string.creating_app_ops_lists;
      default -> 0;
    };
  }

  boolean isPkgUpdated(PackageInfo pkgInfo, Package pkg, boolean filterPerms) {
    Boolean filteredOut = PkgParserFlavor.INS.isFilteredOut(pkgInfo, pkg);
    if (Boolean.TRUE.equals(filteredOut)) {
      return false;
    }

    boolean filterPkg = filteredOut == null;

    if (filterPkg && isFilteredOutPkgName(pkgInfo.packageName)) {
      return false;
    }

    boolean isSystemApp = isSystemApp(pkgInfo);
    if (filterPkg && isFilteredOutSystemPkg(isSystemApp)) {
      return false;
    }

    boolean isFrameworkApp = isFrameworkApp(pkgInfo);
    if (filterPkg && isFilteredOutFrameworkPkg(isFrameworkApp)) {
      return false;
    }
    if (filterPkg && isFilteredOutUserPkg(isFrameworkApp, isSystemApp)) {
      return false;
    }

    ApplicationInfo appInfo = pkgInfo.applicationInfo;
    boolean isEnabled = appInfo.enabled;
    if (filterPkg && isFilteredOutDisabledPkg(!isEnabled)) {
      return false;
    }

    if (filterPkg && isFilteredOutNoIconPkg(appInfo.icon == 0)) {
      return false;
    }

    List<Permission> permList = getPermList(pkgInfo, pkg, filterPerms);

    if (filterPkg && isFilteredOutNoPermPkg(pkg)) {
      return false;
    }

    Boolean pkgIsReferenced = true;

    if (permList.parallelStream().anyMatch(perm -> Boolean.FALSE.equals(perm.isReferenced()))) {
      pkgIsReferenced = false;
    } else if (permList.parallelStream()
        .anyMatch(perm -> perm.isReferenced() == null && perm.isChangeable())) {
      pkgIsReferenced = null;
    }

    pkg.updatePackage(
        appInfo.loadLabel(mPm).toString(),
        pkgInfo.packageName,
        permList,
        isFrameworkApp,
        isSystemApp,
        isEnabled,
        appInfo.uid,
        pkgIsReferenced,
        pkgInfo.firstInstallTime,
        pkgInfo.lastUpdateTime);

    return !filterPkg || !PkgParserFlavor.INS.isFilteredOut(pkg);
  }

  private boolean isSystemApp(PackageInfo packageInfo) {
    return (packageInfo.applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0;
  }

  private List<Integer> mSystemSignatures;

  public List<Integer> getSystemSignatures() {
    if (mSystemSignatures == null) {
      PackageInfo pkgInfo = getPkgInfo("android", null);
      mSystemSignatures =
          Arrays.asList(Util.getPackageSignatures(pkgInfo)).parallelStream()
              .mapToInt(Signature::hashCode)
              .boxed()
              .collect(Collectors.toList());
    }
    return mSystemSignatures;
  }

  private boolean isFrameworkApp(PackageInfo packageInfo) {
    return Util.isFwkPkg(packageInfo, getSystemSignatures());
  }

  private boolean isFilteredOutPkgName(String pkgName) {
    return ExcFiltersData.INS.isPkgExcluded(pkgName);
  }

  private boolean isFilteredOutSystemPkg(boolean isSystemPkg) {
    return MySettings.INS.excludeSystemApps() && isSystemPkg;
  }

  private boolean isFilteredOutFrameworkPkg(boolean isFrameworkPkg) {
    return MySettings.INS.excludeFrameworkApps() && isFrameworkPkg;
  }

  private boolean isFilteredOutUserPkg(boolean isFrameworkPkg, boolean isSystemPkg) {
    return MySettings.INS.excludeUserApps() && !isFrameworkPkg && !isSystemPkg;
  }

  private boolean isFilteredOutDisabledPkg(boolean isDisabledPkg) {
    return MySettings.INS.excludeDisabledApps() && isDisabledPkg;
  }

  private boolean isFilteredOutNoIconPkg(boolean isNoIconPkg) {
    return MySettings.INS.excludeNoIconApps() && isNoIconPkg;
  }

  private boolean isFilteredOutNoPermPkg(Package pkg) {
    return MySettings.INS.excludeNoPermsApps()
        && pkg.getTotalPermCount() == 0
        && pkg.getTotalAppOpsCount() == 0;
  }

  private List<Permission> getPermList(PackageInfo pkgInfo, Package pkg, boolean filterPerms) {
    String[] requestedPerms = pkgInfo.requestedPermissions;
    List<Permission> permList = new ArrayList<>();

    int permCount = 0;
    int[] appOpsCount1 = new int[] {0, 0};
    List<Integer> processedAppOps = new ArrayList<>();

    List<String> filter = pkg.getPermFilter();

    if (requestedPerms != null) {
      for (int count = 0; count < requestedPerms.length; count++) {
        String permName = requestedPerms[count].replaceAll("\\s", "");
        if ((filter == null || filter.contains(permName))
            && createPerm(pkgInfo, permName, count, filterPerms, permList)) {
          permCount++;
        }

        if (AppOpsParser.INS.hasAppOps()) {
          int[] appOpsCount =
              createPermsAppOpsNotSet(
                  pkgInfo, permName, permList, processedAppOps, filter, filterPerms);
          appOpsCount1[0] += appOpsCount[0];
          appOpsCount1[1] += appOpsCount[1];
        }
      }
    }

    int[] appOpsCount2 = new int[] {0, 0};
    int[] appOpsCount3 = new int[] {0, 0};
    if (AppOpsParser.INS.hasAppOps()) {
      appOpsCount2 = createSetAppOps(pkgInfo, permList, processedAppOps, filter, filterPerms);

      if (MySettings.INS.showExtraAppOps()
          && (!MySettings.INS.excludeNoPermsApps()
              || requestedPerms != null
              || appOpsCount2[0] != 0)) {

        List<Integer> ops1 = new ArrayList<>();
        for (String opName : ExcFiltersData.INS.getExtraAppOps()) {
          Integer op = AppOpsParser.INS.getAppOpCode(opName);
          if (op != null && !processedAppOps.contains(op)) {
            ops1.add(op);
          }
        }

        if (!ops1.isEmpty()) {
          int[] ops2 = new int[ops1.size()];
          for (int i = 0; i < ops1.size(); i++) {
            ops2[i] = ops1.get(i);
          }
          appOpsCount3 = createExtraAppOps(pkgInfo, permList, ops2, filter, filterPerms);
        }
      }
    }

    pkg.setTotalPermCount(requestedPerms == null ? 0 : requestedPerms.length);
    pkg.setPermCount(permCount);
    pkg.setTotalAppOpsCount(appOpsCount1[0] + appOpsCount2[0] + appOpsCount3[0]);
    pkg.setAppOpsCount(appOpsCount1[1] + appOpsCount2[1] + appOpsCount3[1]);

    return permList;
  }

  private boolean isNotFilteredOut(Permission perm) {

    if (perm.isExtraAppOp()) {
      return true;
    }

    if (ExcFiltersData.INS.isPermExcluded(perm.getName())) {
      return false;
    }
    if (MySettings.INS.excludeNotChangeablePerms() && !perm.isChangeable()) {
      return false;
    }
    if (MySettings.INS.excludeNotGrantedPerms() && !perm.isGranted()) {
      return false;
    }

    if (perm.isAppOp()) {
      return !MySettings.INS.excludeNotSetAppOps() || perm.isAppOpModeSet();
    }

    if (MySettings.INS.excludePrivilegedPerms() && perm.isPrivileged()) {
      return false;
    }
    if (MySettings.INS.excludeSignaturePerms()
        && perm.getProtectionLevel().equals(Permission.PROTECTION_SIGNATURE)) {
      return false;
    }
    if (MySettings.INS.excludeDangerousPerms()
        && perm.getProtectionLevel().equals(Permission.PROTECTION_DANGEROUS)) {
      return false;
    }
    if (MySettings.INS.excludeNormalPerms()
        && perm.getProtectionLevel().equals(Permission.PROTECTION_NORMAL)) {
      return false;
    }
    return !MySettings.INS.excludeInvalidPerms() || perm.hasProviderPkg();
  }

  private boolean createPerm(
      PackageInfo pkgInfo,
      String permName,
      int count,
      boolean filterPerms,
      List<Permission> permList) {
    int[] requestedPermissionsFlags = pkgInfo.requestedPermissionsFlags;

    boolean isGranted =
        (requestedPermissionsFlags[count] & PackageInfo.REQUESTED_PERMISSION_GRANTED) != 0;

    ManifestPermFlags flags = getManifestPermFlags(permName);

    PermGroupInfo permGroupInfo = PermGroupsMapping.INS.get(permName, false);

    boolean isSystemApp = isSystemApp(pkgInfo);
    boolean isFrameworkApp = isFrameworkApp(pkgInfo);

    boolean isSystemFixed = false, isPolicyFixed = false;

    if (isSystemApp || isFrameworkApp) {
      int permFlags = getPermissionFlags(permName, pkgInfo);
      if (permFlags >= 0 && SYSTEM_FIXED_FLAG != null) {
        isSystemFixed = (permFlags & SYSTEM_FIXED_FLAG) != 0;
      }
      if (permFlags >= 0 && POLICY_FIXED_FLAG != null) {
        isPolicyFixed = (permFlags & POLICY_FIXED_FLAG) != 0;
      }
    }

    Permission perm =
        new Permission(
            permGroupInfo.groupId,
            permGroupInfo.icon,
            pkgInfo.packageName,
            permName,
            isGranted,
            isSystemApp,
            isFrameworkApp,
            flags.protection,
            flags.isPrivileged,
            flags.isDevelopment,
            flags.isManifestPermAppOp,
            isSystemFixed,
            isPolicyFixed,
            flags.providerPkg);

    if (!filterPerms || isNotFilteredOut(perm)) {
      String refState =
          PermsDb.INS.getRef(
              pkgInfo.packageName, permName, false, false, pkgInfo.applicationInfo.uid);

      perm.setReference(Permission.isReferenced(refState, perm.isGranted()), refState);
      permList.add(perm);

      return true;
    }

    return false;
  }

  private int getPermissionFlags(String perm, PackageInfo pkgInfo) {
    if (!DaemonHandler.INS.isDaemonAlive()) {
      return -1;
    }

    Integer flags =
        DaemonIface.INS.getPermFlags(
            perm, pkgInfo.packageName, UserUtils.getUserId(pkgInfo.applicationInfo.uid));

    if (flags != null) {
      return flags;
    }

    return -1;
  }

  private int[] createPermsAppOpsNotSet(
      PackageInfo pkgInfo,
      String permName,
      List<Permission> permList,
      List<Integer> processedAppOps,
      List<String> filter,
      boolean filterPerms) {

    Integer op = AppOpsParser.INS.getPermToOpCode(permName);
    if (op == null) {
      return new int[] {0, 0};
    }

    List<MyPackageOps> pkgOpsList =
        AppOpsParser.INS.getOpsForPackage(pkgInfo.applicationInfo.uid, pkgInfo.packageName, op);

    if (pkgOpsList == null || !pkgOpsList.isEmpty()) {
      return new int[] {0, 0};
    }

    return createAppOp(
        pkgInfo, op, null, permList, processedAppOps, false, false, -1, filter, filterPerms);
  }

  private int[] createSetAppOps(
      PackageInfo packageInfo,
      List<Permission> permissionsList,
      List<Integer> processedAppOps,
      List<String> filter,
      boolean filterPerms) {
    return createAppOpsList(
        packageInfo, permissionsList, processedAppOps, null, filter, filterPerms);
  }

  private int[] createExtraAppOps(
      PackageInfo packageInfo,
      List<Permission> permissionsList,
      int[] ops,
      List<String> filter,
      boolean filterPerms) {
    return createAppOpsList(packageInfo, permissionsList, null, ops, filter, filterPerms);
  }

  private int[] createAppOpsList(
      PackageInfo packageInfo,
      List<Permission> permissionsList,
      List<Integer> processedAppOps,
      int[] ops,
      List<String> filter,
      boolean filterPerms) {

    List<MyPackageOps> pkgOpsList = new ArrayList<>();
    int totalAppOpsCount = 0;
    int appOpsCount = 0;
    boolean isExtraAppOp = ops != null;
    int uid = packageInfo.applicationInfo.uid;

    List<MyPackageOps> list;
    if (isExtraAppOp) {
      for (int op : ops) {
        list = AppOpsParser.INS.getOpsForPackage(uid, packageInfo.packageName, op);
        if (list != null) {
          if (list.isEmpty()) {
            int[] count =
                createAppOp(
                    packageInfo,
                    op,
                    null,
                    permissionsList,
                    processedAppOps,
                    true,
                    false,
                    -1,
                    filter,
                    filterPerms);
            totalAppOpsCount += count[0];
            appOpsCount += count[1];
          } else {
            pkgOpsList.addAll(list);
          }
        }
      }
    } else {
      list = AppOpsParser.INS.getOpsForPackage(uid, packageInfo.packageName, null);
      if (list != null) {
        pkgOpsList.addAll(list);
      }

      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
        list = AppOpsParser.INS.getUidOps(uid);
        if (list != null) {
          pkgOpsList.addAll(list);
        }
      }
    }

    for (MyPackageOps myPackageOps : pkgOpsList) {
      boolean isPerUid = myPackageOps.pkgName == null;
      for (MyPackageOps.MyOpEntry myOpEntry : myPackageOps.opEntryList) {
        int op = myOpEntry.op;
        long lastAccessTime = myOpEntry.lastAccessTime;
        int[] count =
            createAppOp(
                packageInfo,
                op,
                myOpEntry.opMode,
                permissionsList,
                processedAppOps,
                isExtraAppOp,
                isPerUid,
                lastAccessTime,
                filter,
                filterPerms);
        totalAppOpsCount += count[0];
        appOpsCount += count[1];
      }
    }
    return new int[] {totalAppOpsCount, appOpsCount};
  }

  private int[] createAppOp(
      PackageInfo pkgInfo,
      int op,
      Integer opMode,
      List<Permission> permList,
      List<Integer> processedAppOps,
      boolean isExtraAppOp,
      boolean isPerUid,
      long accessTime,
      List<String> filter,
      boolean filterPerms) {
    String opName = AppOpsParser.INS.getAppOpName(op);

    if (opName == null) {
      return new int[] {1, 0};
    }

    if (filter != null && !filter.contains(opName)) {
      return new int[] {1, 0};
    }

    if (Constants.UNKNOWN_OP.equals(opName)) {
      return new int[] {0, 0};
    }

    String dependsOn = AppOpsParser.INS.getDependsOn(op);

    boolean opModeSet = true, validMode = true;
    if (opMode == null) {
      opModeSet = false;
    } else if (!AppOpsParser.INS.isValidAppOpMode(opMode)) {

      mOpModesConsistent = validMode = false;
    }

    if (!opModeSet || !validMode) {
      opMode = AppOpsParser.INS.getOpDefMode(op);
      if (opMode == null || !AppOpsParser.INS.isValidAppOpMode(opMode)) {
        opMode = AppOpsManager.MODE_DEFAULT;
        mOpModesConsistent = validMode = false;
      }
    }

    PermGroupInfo permGroupInfo = PermGroupsMapping.INS.get(opName, true);

    Permission perm =
        new Permission(
            permGroupInfo.groupId,
            permGroupInfo.icon,
            pkgInfo.packageName,
            opName,
            Permission.isAppOpGranted(opMode),
            isSystemApp(pkgInfo),
            isFrameworkApp(pkgInfo),
            isPerUid,
            opModeSet,
            !validMode,
            opMode,
            accessTime,
            dependsOn,
            isExtraAppOp);

    if (!isExtraAppOp) {
      processedAppOps.add(op);
    }

    int appOpsCount = 0;

    if (!filterPerms || isNotFilteredOut(perm)) {
      appOpsCount = 1;
    } else if (!isExtraAppOp && ExcFiltersData.INS.isExtraAppOp(perm.getName())) {
      perm.setExtraAppOp();
      if (isNotFilteredOut(perm)) {
        appOpsCount = 1;
      }
    }

    if (appOpsCount == 1) {
      String refState =
          PermsDb.INS.getRef(
              pkgInfo.packageName, opName, true, isPerUid, pkgInfo.applicationInfo.uid);

      perm.setReference(Permission.isReferenced(refState, opMode), refState);
      permList.add(perm);
    }

    return new int[] {1, appOpsCount};
  }

  static PackageInfo getPkgInfo(String pkgName, Integer pmFlags) {
    int flags = pmFlags == null ? Util.PM_GET_SIGNATURES : Util.PM_GET_SIGNATURES | pmFlags;
    try {
      return ApiUtils.getPkgInfo(pkgName, flags);
    } catch (NameNotFoundException e) {
      MyLog.e(TAG, "getPkgInfo", e.toString());
      return null;
    }
  }

  public static final int PI_PROTECTION_MASK_BASE = PermissionInfo.PROTECTION_MASK_BASE;

  public static int getProtectionLevel(PermissionInfo permInfo) {
    return permInfo.protectionLevel;
  }

  private final Map<String, ManifestPermFlags> mManifestFlags = new HashMap<>();

  private ManifestPermFlags getManifestPermFlags(String permName) {
    ManifestPermFlags flags = mManifestFlags.get(permName);

    if (flags == null) {
      try {
        flags = getManifestPermFlags(PackageParser.INS.mPm.getPermissionInfo(permName, 0));
      } catch (NameNotFoundException ignored) {

        flags = new ManifestPermFlags();
      }
      mManifestFlags.put(permName, flags);
    }

    return flags;
  }

  public static ManifestPermFlags getManifestPermFlags(PermissionInfo permInfo) {
    ManifestPermFlags flags = new ManifestPermFlags();

    int protectionLevel = getProtectionLevel(permInfo) & PI_PROTECTION_MASK_BASE;
    int protectionFlags = getProtectionLevel(permInfo) & ~PI_PROTECTION_MASK_BASE;

    int PROTECTION_SIGNATURE_OR_SYSTEM = PermissionInfo.PROTECTION_SIGNATURE_OR_SYSTEM;

    if (protectionLevel == PermissionInfo.PROTECTION_NORMAL) {
      flags.protection = Permission.PROTECTION_NORMAL;
    } else if (protectionLevel == PermissionInfo.PROTECTION_DANGEROUS) {
      flags.protection = Permission.PROTECTION_DANGEROUS;
    } else if (protectionLevel == PermissionInfo.PROTECTION_SIGNATURE) {
      flags.protection = Permission.PROTECTION_SIGNATURE;
    } else if (protectionLevel == PROTECTION_SIGNATURE_OR_SYSTEM) {
      flags.protection = Permission.PROTECTION_SIGNATURE;
    } else if (VERSION.SDK_INT >= VERSION_CODES.S && protectionLevel == PROTECTION_INTERNAL) {
      flags.protection = Permission.PROTECTION_INTERNAL;
    } else {
      MyLog.e(TAG, "createPerm", "Protection level for " + permInfo.name + ": " + protectionLevel);
    }

    flags.isPrivileged = (protectionFlags & PermissionInfo.PROTECTION_FLAG_PRIVILEGED) != 0;
    flags.isDevelopment =
        protectionLevel == PermissionInfo.PROTECTION_SIGNATURE
            && (protectionFlags & PermissionInfo.PROTECTION_FLAG_DEVELOPMENT) != 0;
    flags.isManifestPermAppOp = (protectionFlags & PermissionInfo.PROTECTION_FLAG_APPOP) != 0;

    flags.providerPkg = permInfo.packageName;

    return flags;
  }

  public static class ManifestPermFlags {
    public String protection = Permission.PROTECTION_UNKNOWN;
    public boolean isPrivileged = false;
    public boolean isDevelopment = false;
    public boolean isManifestPermAppOp = false;
    public String providerPkg = null;
  }

  private @interface PostListStatus {
    int UNDEFINED = 0;
    int FINAL = 1;
    int NOT_FINAL = 2;
  }

  private final SingleParamTask<Integer> mSearchQueryExecutor =
      new SingleParamTask<>(this::doSearchInBg, TAG + "-SearchExecutor");

  public void handleSearchQuery() {
    handleSearchQuery(PostListStatus.UNDEFINED);
  }

  private void handleSearchQuery(int isFinal) {
    if (isFinal == PostListStatus.NOT_FINAL && mSearchQueryExecutor.hasRunningOrPendingTasks()) {

      return;
    }

    if (MySettings.INS.isSearching()) {
      mSearchQueryExecutor.cancelAndSubmit(isFinal, true);
    } else {
      mSearchQueryExecutor.cancel(true);
      clearSearchLists();

      postLivePkgList(mPkgList, isFinal != PostListStatus.NOT_FINAL);

      sendListCompleted(isFinal, -1);
    }
  }

  private final Object SEARCH_BG_LOCK = new Object();

  private void doSearchInBg(int isFinal) {
    synchronized (SEARCH_BG_LOCK) {
      List<Package> origPkgList = new ArrayList<>(mPkgList);
      mSearchPkgList.clear();

      boolean sendProgress = false;
      int size = origPkgList.size();

      for (int i = 0; i < size; i++) {
        if (!isUpdating() && Thread.interrupted()) {
          return;
        }

        Package pkg = origPkgList.get(i);

        if (pkg == null) {
          continue;
        }

        if (MySettings.INS.isDeepSearching()) {
          if (!sendProgress) {
            if (!isUpdating()) {
              sendProgress = true;
              setProgress(origPkgList.size(), true, true);
            }
          } else if (isUpdating()) {
            sendProgress = false;
          }

          if (sendProgress) {
            setProgress(i, false, false);
          }
        }

        updateSearchLists(pkg, false);

        if (isFinal == PostListStatus.UNDEFINED && mDoRepeatUpdates && !mSearchPkgList.isEmpty()) {
          postLivePkgList(mSearchPkgList, false);
        }
      }

      setProgress(size, false, true);

      postLivePkgList(mSearchPkgList, isFinal != PostListStatus.NOT_FINAL);
      sendListCompleted(isFinal, mSearchPkgList.size());
    }
  }

  private final List<Package> mSearchPkgList = Collections.synchronizedList(new ArrayList<>());

  private void updateSearchLists(Package pkg, boolean removeOnly) {
    String queryText = MySettings.INS.getQueryText();
    if (queryText == null) {
      return;
    }
    if (!MySettings.INS.isDeepSearching()) {
      if (pkg.contains(queryText)) {
        if (!removeOnly) {
          mSearchPkgList.add(pkg);
        }
      } else if (removeOnly) {
        removeSearchPackage(pkg);
      }
      return;
    }

    List<Permission> permList = new ArrayList<>();
    int permCount = 0, appOpsCount = 0;
    for (Permission perm : pkg.getFullPermsList()) {
      if (perm.contains(pkg, queryText, true)) {
        permList.add(perm);
        if (perm.isAppOp()) {
          appOpsCount++;
        } else {
          permCount++;
        }
      }
    }

    pkg.setSearchPermList(permList);
    pkg.setSearchPermCount(permCount);
    pkg.setSearchAppOpsCount(appOpsCount);

    if (!permList.isEmpty()) {
      if (!removeOnly) {
        mSearchPkgList.add(pkg);
      }
    } else if (removeOnly) {
      removeSearchPackage(pkg);
    }
  }

  private void removeSearchPackage(Package pkg) {
    if (mSearchPkgList.remove(pkg)) {
      pkg.setPkgRemoved(true);
      postLivePkgList(mSearchPkgList, true);
    }
  }

  private void clearSearchLists() {
    synchronized (SEARCH_BG_LOCK) {
      mSearchPkgList.clear();
      new ArrayList<>(mPkgList).parallelStream().forEach(pkg -> pkg.setSearchPermList(null));
    }
  }
}
