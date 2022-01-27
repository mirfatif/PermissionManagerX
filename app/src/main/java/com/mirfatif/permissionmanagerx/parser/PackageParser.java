package com.mirfatif.permissionmanagerx.parser;

import static android.content.pm.PermissionInfo.PROTECTION_INTERNAL;

import android.annotation.SuppressLint;
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
import android.util.Log;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.PermGroupsMapping.GroupOrderPair;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;
import com.mirfatif.privtasks.hiddenapis.err.HiddenAPIsError;
import com.mirfatif.privtasks.ser.MyPackageOps;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public enum PackageParser {
  INSTANCE;

  private static final String TAG = "PackageParser";

  private final PackageManager mPackageManager = App.getContext().getPackageManager();
  private final PermGroupsMapping mPermGroupsMapping = new PermGroupsMapping();

  private final MutableLiveData<List<Package>> mPackagesListLive = new MutableLiveData<>();
  private final MutableLiveData<Package> mChangedPackage = new MutableLiveData<>();
  private final MutableLiveData<Integer> mProgressMax = new MutableLiveData<>();
  private final MutableLiveData<Integer> mProgressNow = new MutableLiveData<>();

  private final List<PackageInfo> mPackageInfoList = new ArrayList<>();
  private final List<Package> mPackagesList = new ArrayList<>();
  private final Map<String, Integer> mPermIconsResIds = new HashMap<>();
  private final Map<String, String> mPermRefList = new HashMap<>();

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PARSERS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private final ExecutorService mUpdatePackagesExecutor = Executors.newSingleThreadExecutor();
  private Future<?> mUpdatePackagesFuture;

  @SuppressWarnings("UnusedReturnValue")
  public Future<?> updatePackagesList() {
    synchronized (mUpdatePackagesExecutor) {
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "updatePackagesList() called");
      }

      /*
       In case of multiple calls, cancel the previous call if waiting for execution.
       Concurrent calls to Lists cause errors.
      */
      if (mUpdatePackagesFuture != null && !mUpdatePackagesFuture.isDone()) {
        if (MySettings.INSTANCE.isDebug()) {
          Util.debugLog(TAG, "updatePackagesList: cancelling previous call");
        }
        mUpdatePackagesFuture.cancel(true);
      }
      mUpdatePackagesFuture =
          mUpdatePackagesExecutor.submit(
              () -> updatePackagesListInBg(false, MySettings.INSTANCE.shouldDoQuickScan()));
      return mUpdatePackagesFuture;
    }
  }

  private boolean mIsUpdating;
  private final Object UPDATE_PKG_BG_LOCK = new Object();

  private boolean updatePackagesListInBg(boolean isBgDeepScan, boolean quickScan) {
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY);
    synchronized (UPDATE_PKG_BG_LOCK) {
      long startTime = System.currentTimeMillis();
      mIsUpdating = true;

      buildPkgInfoList(isBgDeepScan);
      buildRequiredData(isBgDeepScan);

      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(
            TAG, "updatePackagesListInBg: total packages count: " + mPackageInfoList.size());
      }
      // set progress bar scale ASAP
      setProgress(mPackageInfoList.size(), true, false, isBgDeepScan);

      // using global mPackagesList here might give wrong results in case of concurrent calls
      List<Package> packageList = new ArrayList<>();

      // On new request necessarily send updated packages list at least once
      mDoRepeatUpdates = true;
      newUpdateRequest();

      for (int i = 0; i < mPackageInfoList.size(); i++) {
        // handle concurrent calls
        if (Thread.interrupted()) {
          if (MySettings.INSTANCE.isDebug()) {
            Util.debugLog(TAG, "updatePackagesListInBg: breaking loop, new call received");
          }
          return false;
        }

        if (isBgDeepScan && MySettings.INSTANCE.isSearching()) {
          isBgDeepScan = false;
          setProgress(mPackageInfoList.size(), true, false, false);
        }

        setProgress(i, false, false, isBgDeepScan);
        PackageInfo packageInfo = mPackageInfoList.get(i);
        if (MySettings.INSTANCE.isDebug()) {
          Util.debugLog(
              TAG, "updatePackagesListInBg: updating package: " + packageInfo.packageName);
        }

        Package pkg = new Package();
        if (isPkgUpdated(packageInfo, pkg, quickScan, true)) {
          packageList.add(pkg);
          PkgParserFlavor.INSTANCE.onPkgCreated(pkg);

          if (!isBgDeepScan && shouldDoRepeatUpdates()) {
            submitLiveData(packageList, false);
            mDoRepeatUpdates = false;
          }
        }
      }

      PkgParserFlavor.INSTANCE.sortPkgListAgain(packageList);

      /*
       Finally update complete list and complete progress.
       Packages LiveList must be updated before updating ProgressBars. List
       size might get queried on final progress status in MainActivity.
      */
      submitLiveData(packageList, true);
      setProgress(PKG_PROG_ENDS, false, true, isBgDeepScan);

      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(
            TAG,
            "updatePackagesListInBg: total time: "
                + (System.currentTimeMillis() - startTime)
                + "ms");
      }

      // Build search keys db
      if (quickScan) {
        return updatePackagesListInBg(true, false);
      }

      PkgParserFlavor.INSTANCE.onPkgListCompleted();
      mIsUpdating = false;
      if (MySettings.INSTANCE.isPrivDaemonAlive()) {
        Utils.runInBg(() -> PrivDaemonHandler.INSTANCE.sendRequest(Commands.RESET_OOS));
      }

      return true;
    }
  }

  private long mLastPackageManagerCall = 0;

  private void buildPkgInfoList(boolean isBgDeepScan) {
    if (System.currentTimeMillis() - mLastPackageManagerCall < 5000) {
      return; // Don't trouble Android on every call.
    }

    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "buildPkgInfoList: updating packages list");
    }

    setProgress(PACKAGES_LIST, true, false, isBgDeepScan);

    synchronized (mPackageInfoList) {
      mPackageInfoList.clear();
      mPackageInfoList.addAll(PkgParserFlavor.INSTANCE.getPackageList());
      PkgParserFlavor.INSTANCE.sortPkgList(mPackageInfoList);
    }

    mLastPackageManagerCall = System.currentTimeMillis();
  }

  public void buildRequiredData(boolean isBgDeepScan) {
    // If permissions database changes, manually call buildPermRefList()
    if (mPermRefList.isEmpty()) {
      setProgress(REF_PERMS_LIST, true, false, isBgDeepScan);
      buildPermRefList();
    }

    if (!MySettings.INSTANCE.excludeAppOpsPerms() && MySettings.INSTANCE.canReadAppOps()) {
      setProgress(APP_OPS_LISTS, true, false, isBgDeepScan);
      AppOpsParser.INSTANCE.buildAppOpsLists();
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// GETTERS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  @SuppressWarnings("UnusedDeclaration")
  List<Package> getPackageList() {
    synchronized (mPackagesList) {
      return new ArrayList<>(mPackagesList);
    }
  }

  private int mPkgCount;

  public int getPkgCount() {
    return mPkgCount;
  }

  public Package getPackage(int position) {
    synchronized (mPackagesList) {
      if (position < 0 || position >= mPackagesList.size()) {
        Log.e(TAG, "getPackage: bad position: " + position);
        return null;
      }
      return mPackagesList.get(position);
    }
  }

  public int getPackagePosition(Package pkg) {
    synchronized (mPackagesList) {
      int position = mPackagesList.indexOf(pkg);
      if (position == -1) {
        Log.e(TAG, "getPackagePosition: bad Package provided");
        return -1;
      }
      return position;
    }
  }

  //////////////////////////////////////////////////////////////////
  /////////////////////////// MODIFIERS ////////////////////////////
  //////////////////////////////////////////////////////////////////

  // When calling from MainActivity or PackageActivity for existing package
  public void updatePackage(Package pkg) {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "updatePackage: " + pkg.getLabel());
    }
    PackageInfo packageInfo = PkgParserFlavor.INSTANCE.getPackageInfo(pkg);

    // Package uninstalled, ref states changed, or disabled from MainActivity
    if (packageInfo == null || !isPkgUpdated(packageInfo, pkg, false, true)) {
      removePackage(pkg);
      return;
    }

    // update packages list when a Package's or Permission's state is changed so that RecyclerView
    // is updated on return to MainActivity
    mChangedPackage.postValue(pkg);

    // In case of search, also update temporary Package and Perms lists
    if (MySettings.INSTANCE.isSearching()) {
      updateSearchLists(pkg, true);
    }
  }

  public void removePackage(Package pkg) {
    if (mIsUpdating) {
      updatePackagesList();
      return;
    }
    boolean res;
    synchronized (mPackagesList) {
      res = mPackagesList.remove(pkg);
    }
    if (res) {
      if (MySettings.INSTANCE.isSearching()) {
        removeSearchPackage(pkg);
      } else {
        postLiveData(mPackagesList);
      }
      pkg.setIsRemoved(true);
    } else {
      Log.e(TAG, "removePackage: bad Package provided");
    }
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////// LIVE UPDATES ///////////////////////////
  //////////////////////////////////////////////////////////////////

  public void unsetProgress() {
    Utils.runInBg(() -> mProgressNow.postValue(null));
  }

  public LiveData<List<Package>> getPackagesListLive() {
    updatePackagesList(); // update list on app (re)launch
    return mPackagesListLive;
  }

  public LiveData<Package> getChangedPackage() {
    return mChangedPackage;
  }

  public LiveData<Integer> getProgressMax() {
    return mProgressMax;
  }

  public LiveData<Integer> getProgressNow() {
    return mProgressNow;
  }

  private void submitLiveData(List<Package> packagesList, boolean isFinal) {
    synchronized (mPackagesList) {
      if (packagesList != null) {
        mPackagesList.clear();
        mPackagesList.addAll(packagesList);
      }
    }
    if (!MySettings.INSTANCE.isSearching()) {
      postLiveData(mPackagesList);
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "submitLiveData: empty query text, returning");
      }
      return;
    }
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "submitLiveData: doing search");
    }
    handleSearchQuery(isFinal);
  }

  private void postLiveData(List<Package> packageList) {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "postLiveData: posting " + packageList.size() + " packages");
    }
    mPackagesListLive.postValue(new ArrayList<>(packageList));
    mPkgCount = packageList.size();
    packagesListUpdateTimeStamp = System.currentTimeMillis();
  }

  //////////////////////////////////////////////////////////////////
  /////////////////////////// PROGRESS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private boolean mRepeatUpdates = true, mDoRepeatUpdates = true;

  private boolean shouldDoRepeatUpdates() {
    return mRepeatUpdates || mDoRepeatUpdates;
  }

  public void setRepeatUpdates(boolean repeatUpdates) {
    if (repeatUpdates) {
      mDoRepeatUpdates = true;
    }
    mRepeatUpdates = repeatUpdates;
  }

  private long mLastProgressTimeStamp = 0;

  private void setProgress(int value, boolean isMax, boolean isFinal, boolean isBgDeepScan) {
    if (isBgDeepScan) {
      return;
    }
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "setProgress: value: " + value + ", isMax: " + isMax);
    }

    if (isMax) {
      mProgressMax.postValue(value);
      PkgParserFlavor.INSTANCE.setProgress(true, value);
      return;
    }

    if (isFinal) {
      mProgressNow.postValue(value);
      PkgParserFlavor.INSTANCE.setProgress(false, value);
      return;
    }

    // set progress updates, but not too frequent
    if ((System.currentTimeMillis() - mLastProgressTimeStamp) > 100) {
      mProgressNow.postValue(value);
      PkgParserFlavor.INSTANCE.setProgress(false, value);
      mLastProgressTimeStamp = System.currentTimeMillis();
    }
  }

  private long packagesListUpdateTimeStamp = 0;

  // do not update RecyclerView too frequently
  private boolean shouldUpdateLiveData() {
    return (System.currentTimeMillis() - packagesListUpdateTimeStamp) > 100;
  }

  // Do not fluctuate UI unnecessarily
  public void newUpdateRequest() {
    packagesListUpdateTimeStamp = System.currentTimeMillis() + 500;
  }

  // to show progress
  private static final int PACKAGES_LIST = -1;
  private static final int REF_PERMS_LIST = -2;
  private static final int APP_OPS_LISTS = -3;
  public static final int PKG_PROG_ENDS = -4;
  public static final int SEARCH_ENDS = -5;

  public int getProgressTextResId(int progressMax) {
    switch (progressMax) {
      case PACKAGES_LIST:
        return R.string.creating_packages_list;
      case REF_PERMS_LIST:
        return R.string.reading_reference_perms;
      case APP_OPS_LISTS:
        return R.string.creating_app_ops_lists;
    }
    return 0;
  }

  //////////////////////////////////////////////////////////////////
  /////////////////////////// PACKAGES /////////////////////////////
  //////////////////////////////////////////////////////////////////

  @SuppressWarnings("SameParameterValue")
  boolean isPkgUpdated(
      PackageInfo packageInfo, Package pkg, boolean quickScan, boolean applyFilter) {
    boolean shouldFilterOut = true;
    if (applyFilter) {
      if (PkgParserFlavor.INSTANCE.isFilteredOut(packageInfo, pkg)) {
        return false;
      } else {
        shouldFilterOut = PkgParserFlavor.INSTANCE.shouldFilterOut();
      }
    }

    if (shouldFilterOut && isFilteredOutPkgName(packageInfo.packageName)) {
      return false;
    }

    boolean isSystemApp = isSystemApp(packageInfo);
    if (shouldFilterOut && isFilteredOutSystemPkg(isSystemApp)) {
      return false;
    }

    boolean isFrameworkApp = isFrameworkApp(packageInfo);
    if (shouldFilterOut && isFilteredOutFrameworkPkg(isFrameworkApp)) {
      return false;
    }
    if (shouldFilterOut && isFilteredOutUserPkg(isFrameworkApp, isSystemApp)) {
      return false;
    }

    ApplicationInfo appInfo = packageInfo.applicationInfo;
    boolean isEnabled = appInfo.enabled;
    if (shouldFilterOut && isFilteredOutDisabledPkg(!isEnabled)) {
      return false;
    }

    if (shouldFilterOut && isFilteredOutNoIconPkg(appInfo.icon == 0)) {
      return false;
    }

    List<Permission> permissionsList = new ArrayList<>();
    Boolean pkgIsReferenced = true;

    if (!quickScan) {
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "isPkgUpdated: building permissions list");
      }
      permissionsList = getPermissionsList(packageInfo, pkg);

      // Exclude packages with no manifest permissions and no AppOps (excluding extra)
      if (shouldFilterOut && isFilteredOutNoPermPkg(pkg)) {
        return false;
      }

      for (Permission perm : permissionsList) {
        if (perm.isReferenced() != null && !perm.isReferenced()) {
          pkgIsReferenced = false;
          break;
        }
      }

      if (pkgIsReferenced) {
        for (Permission perm : permissionsList) {
          if (perm.isReferenced() == null && perm.isChangeable()) {
            pkgIsReferenced = null;
            break;
          }
        }
      }
    }

    pkg.updatePackage(
        appInfo.loadLabel(mPackageManager).toString(),
        packageInfo.packageName,
        permissionsList,
        isFrameworkApp,
        isSystemApp,
        isEnabled,
        appInfo.uid,
        pkgIsReferenced,
        packageInfo.firstInstallTime,
        new File(appInfo.sourceDir).lastModified());

    if (shouldFilterOut && applyFilter && PkgParserFlavor.INSTANCE.isFilteredOut(pkg)) {
      return false;
    }

    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "isPkgUpdated: Package created");
    }

    return true;
  }

  private boolean isSystemApp(PackageInfo packageInfo) {
    return (packageInfo.applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0;
  }

  private List<Signature> mSystemSignatures;

  public boolean isFrameworkApp(PackageInfo packageInfo) {
    if (mSystemSignatures == null) {
      PackageInfo pkgInfo = getPackageInfo("android", null);
      if (pkgInfo != null) {
        mSystemSignatures = Arrays.asList(getPackageSignatures(pkgInfo));
      } else {
        return false;
      }
    }
    for (Signature signature : getPackageSignatures(packageInfo)) {
      if (mSystemSignatures.contains(signature)) {
        return true;
      }
    }
    return false;
  }

  private boolean isFilteredOutPkgName(String pkgName) {
    return MySettings.INSTANCE.isPkgExcluded(pkgName);
  }

  private boolean isFilteredOutSystemPkg(boolean isSystemPkg) {
    return MySettings.INSTANCE.excludeSystemApps() && isSystemPkg;
  }

  private boolean isFilteredOutFrameworkPkg(boolean isFrameworkPkg) {
    return MySettings.INSTANCE.excludeFrameworkApps() && isFrameworkPkg;
  }

  private boolean isFilteredOutUserPkg(boolean isFrameworkPkg, boolean isSystemPkg) {
    return MySettings.INSTANCE.excludeUserApps() && !isFrameworkPkg && !isSystemPkg;
  }

  private boolean isFilteredOutDisabledPkg(boolean isDisabledPkg) {
    return MySettings.INSTANCE.excludeDisabledApps() && isDisabledPkg;
  }

  private boolean isFilteredOutNoIconPkg(boolean isNoIconPkg) {
    return MySettings.INSTANCE.excludeNoIconApps() && isNoIconPkg;
  }

  private boolean isFilteredOutNoPermPkg(Package pkg) {
    return MySettings.INSTANCE.shouldExcludeNoPermApps()
        && pkg.getTotalPermCount() == 0
        && pkg.getTotalAppOpsCount() == 0;
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////////// PERMISSIONS ///////////////////////////
  //////////////////////////////////////////////////////////////////

  // Update changed package and permissions from PackageActivity.
  // Calls to Room database require background execution and are time taking too.
  public void updatePermReferences(String pkgName, String permName, String state) {
    mPermRefList.remove(pkgName + "_" + permName);
    if (state != null) {
      mPermRefList.put(pkgName + "_" + permName, state);
    }
  }

  public void buildPermRefList() {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "buildPermRefList() called");
    }
    synchronized (mPermRefList) {
      mPermRefList.clear();
      for (PermissionEntity entity : MySettings.INSTANCE.getPermDb().getAll()) {
        mPermRefList.put(entity.pkgName + "_" + entity.permName, entity.state);
      }
    }
  }

  private List<Permission> getPermissionsList(PackageInfo packageInfo, Package pkg) {
    String[] requestedPermissions = packageInfo.requestedPermissions;
    List<Permission> permissionsList = new ArrayList<>();

    Permission permission;
    int permCount = 0;
    int[] appOpsCount1 = new int[] {0, 0};
    List<Integer> processedAppOps = new ArrayList<>();

    List<String> filter = pkg.getPermFilter();

    if (requestedPermissions != null) {
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "getPermissionsList: parsing permissions list");
      }
      for (int count = 0; count < requestedPermissions.length; count++) {
        String perm = requestedPermissions[count].replaceAll("\\s", "");
        if (filter == null || filter.contains(perm)) {
          permission = createPermission(packageInfo, perm, count);
          if (isNotFilteredOut(permission)) {
            permissionsList.add(permission);
            permCount++;
          }
        }

        // not set AppOps corresponding to manifest permission
        if (!MySettings.INSTANCE.excludeAppOpsPerms() && MySettings.INSTANCE.canReadAppOps()) {
          int[] appOpsCount =
              createPermsAppOpsNotSet(packageInfo, perm, permissionsList, processedAppOps, filter);
          appOpsCount1[0] += appOpsCount[0];
          appOpsCount1[1] += appOpsCount[1];
        }
      }
    }

    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "getPermissionsList: parsing AppOps");
    }

    int[] appOpsCount2 = new int[] {0, 0};
    int[] appOpsCount3 = new int[] {0, 0};
    if (!MySettings.INSTANCE.excludeAppOpsPerms() && MySettings.INSTANCE.canReadAppOps()) {
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(
            TAG, "getPermissionsList: parsing AppOps not corresponding to any manifest permission");
      }
      appOpsCount2 = createSetAppOps(packageInfo, permissionsList, processedAppOps, filter);

      /*
        Do not count extra AppOps if app has no manifest permission and no other AppOp.
        Otherwise no apps will be excluded on excludeNoPermissionsApps() basis if even one extra
        AppOps is selected in list.
        In case of quick scan excludeNoPermissionsApps() is ignored, so show all.
      */
      if (MySettings.INSTANCE.showExtraAppOps()
          && (!MySettings.INSTANCE.shouldExcludeNoPermApps()
              || requestedPermissions != null
              || appOpsCount2[0] != 0)) {

        if (MySettings.INSTANCE.isDebug()) {
          Util.debugLog(TAG, "getPermissionsList: parsing extra AppOps");
        }

        // Irrelevant / extra AppOps, not set and not corresponding to any manifest permission
        List<Integer> ops1 = new ArrayList<>();
        for (String opName : MySettings.INSTANCE.getExtraAppOps()) {
          int op = AppOpsParser.INSTANCE.getAppOpsList().indexOf(opName);
          if (!processedAppOps.contains(op)) {
            ops1.add(op);
          }
        }

        if (ops1.size() != 0) {
          int[] ops2 = new int[ops1.size()];
          for (int i = 0; i < ops1.size(); i++) {
            ops2[i] = ops1.get(i);
          }
          appOpsCount3 = createExtraAppOps(packageInfo, permissionsList, ops2, filter);
        }
      }
    }

    pkg.setTotalPermCount(requestedPermissions == null ? 0 : requestedPermissions.length);
    pkg.setPermCount(permCount);
    pkg.setTotalAppOpsCount(appOpsCount1[0] + appOpsCount2[0] + appOpsCount3[0]);
    pkg.setAppOpsCount(appOpsCount1[1] + appOpsCount2[1] + appOpsCount3[1]);

    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "getPermissionsList: permissions count: " + permissionsList.size());
    }

    return permissionsList;
  }

  private boolean isNotFilteredOut(Permission permission) {
    // always show extra AppOps except in search query
    if (permission.isExtraAppOp()) {
      return true;
    }

    if (MySettings.INSTANCE.isPermExcluded(permission.getName())) {
      return false;
    }
    if (MySettings.INSTANCE.excludeNotChangeablePerms() && !permission.isChangeable()) {
      return false;
    }
    if (MySettings.INSTANCE.excludeNotGrantedPerms() && !permission.isGranted()) {
      return false;
    }

    if (permission.isAppOps()) {
      return !MySettings.INSTANCE.excludeNotSetAppOps() || permission.isAppOpsSet();
    }

    if (MySettings.INSTANCE.excludePrivilegedPerms() && permission.isPrivileged()) {
      return false;
    }
    if (MySettings.INSTANCE.excludeSignaturePerms()
        && permission.getProtectionLevel().equals(Permission.PROTECTION_SIGNATURE)) {
      return false;
    }
    if (MySettings.INSTANCE.excludeDangerousPerms()
        && permission.getProtectionLevel().equals(Permission.PROTECTION_DANGEROUS)) {
      return false;
    }
    if (MySettings.INSTANCE.excludeNormalPerms()
        && permission.getProtectionLevel().equals(Permission.PROTECTION_NORMAL)) {
      return false;
    }
    return !MySettings.INSTANCE.excludeInvalidPermissions() || !permission.isProviderMissing();
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////// MANIFEST PERMISSION ///////////////////////
  //////////////////////////////////////////////////////////////////

  private Permission createPermission(PackageInfo packageInfo, String perm, int count) {
    int[] requestedPermissionsFlags = packageInfo.requestedPermissionsFlags;
    String protection = Permission.PROTECTION_UNKNOWN;
    boolean isPrivileged = false;
    boolean isDevelopment = false;
    boolean isManifestPermAppOp = false;
    boolean isSystemFixed = false, isPolicyFixed = false;
    boolean providerMissing = false;
    CharSequence permDesc = null;

    boolean isGranted =
        (requestedPermissionsFlags[count] & PackageInfo.REQUESTED_PERMISSION_GRANTED) != 0;

    try {
      PermissionInfo permissionInfo = mPackageManager.getPermissionInfo(perm, 0);

      int protectionLevel = getProtLevel(permissionInfo) & PI_PROTECTION_MASK_BASE;
      int protectionFlags = getProtLevel(permissionInfo) & ~PI_PROTECTION_MASK_BASE;
      @SuppressWarnings("deprecation")
      int PROTECTION_SIGNATURE_OR_SYSTEM = PermissionInfo.PROTECTION_SIGNATURE_OR_SYSTEM;

      if (protectionLevel == PermissionInfo.PROTECTION_NORMAL) {
        protection = Permission.PROTECTION_NORMAL;
      } else if (protectionLevel == PermissionInfo.PROTECTION_DANGEROUS) {
        protection = Permission.PROTECTION_DANGEROUS;
      } else if (protectionLevel == PermissionInfo.PROTECTION_SIGNATURE) {
        protection = Permission.PROTECTION_SIGNATURE;
      } else if (protectionLevel == PROTECTION_SIGNATURE_OR_SYSTEM) {
        protection = Permission.PROTECTION_SIGNATURE;
      } else if (VERSION.SDK_INT >= VERSION_CODES.S && protectionLevel == PROTECTION_INTERNAL) {
        protection = Permission.PROTECTION_INTERNAL;
      } else {
        Log.e(
            TAG,
            "createPermission: protection level for "
                + packageInfo.packageName
                + ": "
                + permissionInfo.name
                + ": "
                + protectionLevel);
      }

      isPrivileged = (protectionFlags & PermissionInfo.PROTECTION_FLAG_PRIVILEGED) != 0;
      isDevelopment =
          protectionLevel == PermissionInfo.PROTECTION_SIGNATURE
              && (protectionFlags & PermissionInfo.PROTECTION_FLAG_DEVELOPMENT) != 0;
      isManifestPermAppOp = (protectionFlags & PermissionInfo.PROTECTION_FLAG_APPOP) != 0;

      permDesc = permissionInfo.loadDescription(mPackageManager);
    } catch (NameNotFoundException ignored) {
      // permissions provider is not available e.g. Play Services
      providerMissing = true;
    }

    String permState = isGranted ? Permission.GRANTED : Permission.REVOKED;
    RefPair refPair = getReference(packageInfo.packageName, perm, permState);
    GroupOrderPair groupOrderPair = mPermGroupsMapping.getOrderAndGroup(perm, false);

    boolean isSystemApp = isSystemApp(packageInfo);
    boolean isFrameworkApp = isFrameworkApp(packageInfo);
    if (isSystemApp || isFrameworkApp) {
      int permFlags = getPermissionFlags(perm, packageInfo);
      if (permFlags >= 0 && getSystemFixedFlag() != null) {
        isSystemFixed = (permFlags & getSystemFixedFlag()) != 0;
      }
      if (permFlags >= 0 && getPolicyFixedFlag() != null) {
        isPolicyFixed = (permFlags & getPolicyFixedFlag()) != 0;
      }
    }

    return new Permission(
        groupOrderPair.order,
        getIconResId(perm, groupOrderPair.group),
        packageInfo.packageName,
        perm,
        isGranted,
        refPair.isReferenced,
        refPair.reference,
        isSystemApp,
        isFrameworkApp,
        protection,
        isPrivileged,
        isDevelopment,
        isManifestPermAppOp,
        isSystemFixed,
        isPolicyFixed,
        providerMissing,
        permDesc);
  }

  private int getPermissionFlags(String perm, PackageInfo packageInfo) {
    if (!MySettings.INSTANCE.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getPermissionFlags");
      return -1;
    } else {
      String command =
          Commands.GET_PERMISSION_FLAGS
              + " "
              + perm
              + " "
              + packageInfo.packageName
              + " "
              + Utils.getUserId(packageInfo.applicationInfo.uid);
      Object object = PrivDaemonHandler.INSTANCE.sendRequest(command);
      if (object instanceof Integer) {
        return (int) object;
      }
    }
    Log.e(TAG, "Error occurred in getPermissionFlags()");
    return -1;
  }

  private Integer SYSTEM_FIXED_FLAG = null;

  public Integer getSystemFixedFlag() {
    if (SYSTEM_FIXED_FLAG != null) {
      return SYSTEM_FIXED_FLAG;
    }

    try {
      SYSTEM_FIXED_FLAG = HiddenAPIs.getSystemFixedFlag();
      return SYSTEM_FIXED_FLAG;
    } catch (HiddenAPIsError e) {
      Log.e(TAG, "getSystemFixedFlag: " + e.toString());
    }

    if (!MySettings.INSTANCE.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getSystemFixedFlag");
      return SYSTEM_FIXED_FLAG;
    }

    Object object = PrivDaemonHandler.INSTANCE.sendRequest(Commands.GET_SYSTEM_FIXED_FLAG);
    if (object instanceof Integer) {
      SYSTEM_FIXED_FLAG = (Integer) object;
      return SYSTEM_FIXED_FLAG;
    }

    Log.e(TAG, "Error occurred in getSystemFixedFlag()");
    return SYSTEM_FIXED_FLAG;
  }

  private Integer POLICY_FIXED_FLAG = null;

  private Integer getPolicyFixedFlag() {
    if (POLICY_FIXED_FLAG != null) {
      return POLICY_FIXED_FLAG;
    }

    try {
      POLICY_FIXED_FLAG = HiddenAPIs.getPolicyFixedFlag();
      return POLICY_FIXED_FLAG;
    } catch (HiddenAPIsError e) {
      Log.e(TAG, "getPolicyFixedFlag: " + e.toString());
    }

    if (!MySettings.INSTANCE.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getPolicyFixedFlag");
      return POLICY_FIXED_FLAG;
    }

    Object object = PrivDaemonHandler.INSTANCE.sendRequest(Commands.GET_POLICY_FIXED_FLAG);
    if (object instanceof Integer) {
      POLICY_FIXED_FLAG = (int) object;
      return POLICY_FIXED_FLAG;
    }

    Log.e(TAG, "Error occurred in getPolicyFixedFlag()");
    return POLICY_FIXED_FLAG;
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// APP OPS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private int[] createPermsAppOpsNotSet(
      PackageInfo packageInfo,
      String perm,
      List<Permission> permissionsList,
      List<Integer> processedAppOps,
      List<String> filter) {

    Integer mappedOp = AppOpsParser.INSTANCE.getPermToOpCodeMap().get(perm);
    if (mappedOp == null) {
      return new int[] {0, 0};
    }
    int op = mappedOp;

    List<MyPackageOps> pkgOpsList =
        AppOpsParser.INSTANCE.getOpsForPackage(
            packageInfo.applicationInfo.uid, packageInfo.packageName, op);

    // do not return changed (set) ops, they are handled separately
    if (pkgOpsList != null && pkgOpsList.size() == 0) {
      return createAppOp(
          packageInfo, op, -1, permissionsList, processedAppOps, false, false, -1, filter);
    }

    return new int[] {0, 0};
  }

  private int[] createSetAppOps(
      PackageInfo packageInfo,
      List<Permission> permissionsList,
      List<Integer> processedAppOps,
      List<String> filter) {
    return createAppOpsList(packageInfo, permissionsList, processedAppOps, null, filter);
  }

  private int[] createExtraAppOps(
      PackageInfo packageInfo, List<Permission> permissionsList, int[] ops, List<String> filter) {
    return createAppOpsList(packageInfo, permissionsList, null, ops, filter);
  }

  private int[] createAppOpsList(
      PackageInfo packageInfo,
      List<Permission> permissionsList,
      List<Integer> processedAppOps,
      int[] ops,
      List<String> filter) {

    List<MyPackageOps> pkgOpsList = new ArrayList<>();
    int totalAppOpsCount = 0;
    int appOpsCount = 0;
    boolean isExtraAppOp = ops != null;
    int uid = packageInfo.applicationInfo.uid;

    List<MyPackageOps> list;
    if (isExtraAppOp) {
      for (int op : ops) {
        list = AppOpsParser.INSTANCE.getOpsForPackage(uid, packageInfo.packageName, op);
        if (list != null) {
          if (list.size() == 0) {
            int[] count =
                createAppOp(
                    packageInfo, op, -1, permissionsList, processedAppOps, true, false, -1, filter);
            totalAppOpsCount += count[0];
            appOpsCount += count[1];
          } else {
            pkgOpsList.addAll(list);
          }
        }
      }
    } else {
      list = AppOpsParser.INSTANCE.getOpsForPackage(uid, packageInfo.packageName, null);
      if (list != null) {
        pkgOpsList.addAll(list);
      }

      // UID mode: android-10.0.0_r1: AppOpsService.java#3378
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
        list = AppOpsParser.INSTANCE.getUidOps(uid);
        if (list != null) {
          pkgOpsList.addAll(list);
        }
      }
    }

    for (MyPackageOps myPackageOps : pkgOpsList) {
      boolean isPerUid = myPackageOps.getPackageName() == null;
      for (MyPackageOps.MyOpEntry myOpEntry : myPackageOps.getOps()) {
        int op = myOpEntry.getOp();
        long lastAccessTime = myOpEntry.getLastAccessTime();
        int[] count =
            createAppOp(
                packageInfo,
                op,
                myOpEntry.getMode(),
                permissionsList,
                processedAppOps,
                isExtraAppOp,
                isPerUid,
                lastAccessTime,
                filter);
        totalAppOpsCount += count[0];
        appOpsCount += count[1];
      }
    }
    return new int[] {totalAppOpsCount, appOpsCount};
  }

  private int[] createAppOp(
      PackageInfo packageInfo,
      int op,
      int opMode,
      List<Permission> permissionsList,
      List<Integer> processedAppOps,
      boolean isExtraAppOp,
      boolean isPerUid,
      long accessTime,
      List<String> filter) {
    String opName = AppOpsParser.INSTANCE.getAppOpsList().get(op);
    if (filter != null && !filter.contains(opName)) {
      return new int[] {1, 0};
    }

    int opSwitch = AppOpsParser.INSTANCE.getOpToSwitchList().get(op);
    String dependsOn = op == opSwitch ? null : AppOpsParser.INSTANCE.getAppOpsList().get(opSwitch);
    boolean isAppOpSet = true;
    // Mode can be greater than the modes list we have built. E.g. MODE_ASK in LOS N.
    if (opMode < 0 || opMode >= AppOpsParser.INSTANCE.getAppOpsModes().size()) {
      isAppOpSet = false;
      opMode = AppOpsParser.INSTANCE.getOpToDefModeList().get(op);
    }
    String opState = AppOpsParser.INSTANCE.getAppOpsModes().get(opMode);
    RefPair refPair = getReference(packageInfo.packageName, opName, opState);
    GroupOrderPair groupOrderPair = mPermGroupsMapping.getOrderAndGroup(opName, true);

    Permission permission =
        new Permission(
            groupOrderPair.order,
            getIconResId(opName, groupOrderPair.group),
            packageInfo.packageName,
            opName,
            opMode != AppOpsManager.MODE_IGNORED && opMode != AppOpsManager.MODE_ERRORED,
            refPair.isReferenced,
            refPair.reference,
            isSystemApp(packageInfo),
            isFrameworkApp(packageInfo),
            isPerUid,
            isAppOpSet,
            opMode,
            accessTime,
            dependsOn,
            isExtraAppOp);

    // so that it's not repeated
    if (!isExtraAppOp) {
      processedAppOps.add(op);
    }

    int appOpsCount = 0;

    if (isNotFilteredOut(permission)) {
      permissionsList.add(permission);
      appOpsCount = 1;
    } else if (!isExtraAppOp && MySettings.INSTANCE.isExtraAppOp(permission.getName())) {
      permission.setExtraAppOp();
      if (isNotFilteredOut(permission)) {
        permissionsList.add(permission);
        appOpsCount = 1;
      }
    }

    return new int[] {1, appOpsCount};
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////// HELPER METHODS /////////////////////////
  //////////////////////////////////////////////////////////////////

  @SuppressLint("PackageManagerGetSignatures")
  PackageInfo getPackageInfo(String pkgName, Integer pmFlags) {
    int flags = pmFlags == null ? PM_GET_SIGNATURES : PM_GET_SIGNATURES | pmFlags;
    try {
      return mPackageManager.getPackageInfo(pkgName, flags);
    } catch (NameNotFoundException e) {
      Log.e(TAG, "getPackageInfo: " + e.toString());
      return null;
    }
  }

  public static final int PM_GET_SIGNATURES = PackageManager.GET_SIGNATURES;
  public static final int PI_PROTECTION_MASK_BASE = PermissionInfo.PROTECTION_MASK_BASE;

  public static Signature[] getPackageSignatures(PackageInfo packageInfo) {
    return packageInfo.signatures;
  }

  public static int getProtLevel(PermissionInfo permInfo) {
    return permInfo.protectionLevel;
  }

  private Integer getIconResId(String perm, String group) {
    Integer iconResId = mPermIconsResIds.get(perm);
    if (iconResId == null) {
      iconResId =
          Utils.getStaticIntField(
              "g_" + group.toLowerCase(), R.drawable.class, TAG + ": getIconResId()");
      if (iconResId == null) {
        return null;
      }
      mPermIconsResIds.put(perm, iconResId);
    }
    return iconResId;
  }

  private RefPair getReference(String pkgName, String permName, String state) {
    String refState = mPermRefList.get(pkgName + "_" + permName);
    if (refState == null) {
      return new RefPair(); // both values null
    }
    RefPair refPair = new RefPair();
    refPair.isReferenced = state.equals(refState);
    refPair.reference = refState;
    return refPair;
  }

  private static class RefPair {

    Boolean isReferenced;
    String reference;
  }

  @SuppressWarnings("UnusedDeclaration")
  public void clearPmCallTs() {
    mLastPackageManagerCall = 0;
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// SEARCH /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private final ExecutorService mSearchQueryExecutor = Executors.newSingleThreadExecutor();
  private Future<?> mSearchQueryFuture;

  public void handleSearchQuery() {
    handleSearchQuery(null);
  }

  private void handleSearchQuery(Boolean isFinal) {
    synchronized (mSearchQueryExecutor) {
      if (mSearchQueryFuture != null && !mSearchQueryFuture.isDone()) {
        if (Boolean.FALSE.equals(isFinal)) {
          return;
        }
        if (MySettings.INSTANCE.isDebug()) {
          Util.debugLog(TAG, "handleSearchQuery: cancelling previous call");
        }
        mSearchQueryFuture.cancel(true);
      }

      if (MySettings.INSTANCE.isSearching()) {
        mSearchQueryFuture = mSearchQueryExecutor.submit(() -> doSearchInBg(isFinal));
      } else {
        clearSearchLists();
        postLiveData(mPackagesList);
        if (MySettings.INSTANCE.isDebug()) {
          Util.debugLog(TAG, "handleSearchQuery: empty query text, releasing searchPermLists");
        }
        showSearchEnds(isFinal);
      }
    }
  }

  private final Object SEARCH_BG_LOCK = new Object();

  private void doSearchInBg(Boolean isFinal) {
    synchronized (SEARCH_BG_LOCK) {
      List<Package> origPkgList;
      synchronized (mPackagesList) {
        origPkgList = new ArrayList<>(mPackagesList);
      }

      synchronized (mSearchPkgList) {
        mSearchPkgList.clear();
      }

      boolean sendProgress = false;

      for (int i = 0; i < origPkgList.size(); i++) {
        if (!mIsUpdating && Thread.interrupted()) {
          if (MySettings.INSTANCE.isDebug()) {
            Util.debugLog(TAG, "doSearchInBg: breaking loop, new call received");
          }
          return;
        }

        Package pkg = origPkgList.get(i);

        if (MySettings.INSTANCE.isDeepSearching()) {
          if (!sendProgress) {
            if (!mIsUpdating) {
              sendProgress = true;
              setProgress(origPkgList.size(), true, false, false);
            }
          } else if (mIsUpdating) {
            sendProgress = false;
          }
          if (sendProgress) {
            setProgress(i, false, false, false);
          }
        }
        updateSearchLists(pkg, false);
        if (isFinal == null && shouldUpdateLiveData()) {
          postLiveData(mSearchPkgList);
        }
      }
      postLiveData(mSearchPkgList);
      showSearchEnds(isFinal);
    }
  }

  private final List<Package> mSearchPkgList = new ArrayList<>();

  private void updateSearchLists(Package pkg, boolean removeOnly) {
    String queryText = MySettings.INSTANCE.getQueryText();
    if (!MySettings.INSTANCE.isDeepSearching()) {
      if (pkg.contains(queryText)) {
        if (!removeOnly) {
          synchronized (mSearchPkgList) {
            mSearchPkgList.add(pkg);
          }
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
        if (perm.isAppOps()) {
          appOpsCount++;
        } else {
          permCount++;
        }
      }
    }

    // SearchPermsList must be updated even if empty e.g. when in PackageActivity
    // and Perm is removed because of changed Ref state.
    pkg.setSearchPermList(permList);
    pkg.setSearchPermCount(permCount);
    pkg.setSearchAppOpsCount(appOpsCount);

    if (!permList.isEmpty()) {
      if (!removeOnly) {
        synchronized (mSearchPkgList) {
          mSearchPkgList.add(pkg);
        }
      }
    } else if (removeOnly) {
      removeSearchPackage(pkg);
    }
  }

  private void removeSearchPackage(Package pkg) {
    synchronized (mSearchPkgList) {
      if (mSearchPkgList.remove(pkg)) {
        pkg.setIsRemoved(true);
        postLiveData(mSearchPkgList);
      }
    }
  }

  private void clearSearchLists() {
    synchronized (SEARCH_BG_LOCK) {
      List<Package> origPkgList;
      synchronized (mSearchPkgList) {
        mSearchPkgList.clear();
      }
      synchronized (mPackagesList) {
        origPkgList = new ArrayList<>(mPackagesList);
      }
      for (Package pkg : origPkgList) {
        pkg.setSearchPermList(null);
      }
    }
  }

  // Hide bottom ProgressBar and/or show final Toast with visible apps count.
  private void showSearchEnds(Boolean isFinal) {
    if (isFinal == null ? !mIsUpdating : isFinal) {
      setProgress(SEARCH_ENDS, false, true, false);
    }
  }
}
