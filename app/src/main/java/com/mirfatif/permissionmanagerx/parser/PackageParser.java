package com.mirfatif.permissionmanagerx.parser;

import android.annotation.SuppressLint;
import android.app.AppOpsManager;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PermissionInfo;
import android.content.pm.Signature;
import android.os.Build;
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
import com.mirfatif.privtasks.MyPackageOps;
import com.mirfatif.privtasks.Util;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIsError;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class PackageParser {

  private static final String TAG = "PackageParser";
  private static PackageParser mPackageParser;

  // Create singleton instance of PackageParser so that all activities can update mPackagesListLive
  // whenever needed
  public static synchronized PackageParser getInstance() {
    if (mPackageParser == null) {
      mPackageParser = new PackageParser();
    }
    return mPackageParser;
  }

  private PackageParser() {}

  private final PackageManager mPackageManager = App.getContext().getPackageManager();
  private final MySettings mMySettings = MySettings.getInstance();
  private final AppOpsParser mAppOpsParser = AppOpsParser.getInstance();
  private final PkgParserFlavor mPkgParserFlavor = PkgParserFlavor.getInstance();
  private final PrivDaemonHandler mPrivDaemonHandler = PrivDaemonHandler.getInstance();
  private final PermGroupsMapping mPermGroupsMapping = new PermGroupsMapping();

  private final MutableLiveData<List<Package>> mPackagesListLive = new MutableLiveData<>();
  private final MutableLiveData<Package> mChangedPackage = new MutableLiveData<>();
  private final MutableLiveData<Integer> mProgressMax = new MutableLiveData<>();
  private final MutableLiveData<Integer> mProgressNow = new MutableLiveData<>();

  private List<PackageInfo> mPackageInfoList;
  private final List<Package> mPackagesList = new ArrayList<>();
  private List<Signature> systemSignatures;
  private final Map<String, Integer> mPermIconsResIds = new HashMap<>();
  private List<Integer> mOpToSwitchList;
  private List<Integer> mOpToDefModeList;
  private Map<String, String> mPermRefList;
  private Map<String, Integer> mPermToOpCodeMap;

  private static final int PM_GET_SIGNATURES = PackageManager.GET_SIGNATURES;

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PARSERS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private long mLastPackageManagerCall = 0;
  private final ExecutorService mUpdatePackagesExecutor = Executors.newSingleThreadExecutor();
  private Future<?> mUpdatePackagesFuture;

  public void updatePackagesList() {
    synchronized (mUpdatePackagesExecutor) {
      if (mMySettings.isDebug()) {
        Util.debugLog(TAG, "updatePackagesList() called");
      }

      /**
       * In case of multiple calls, cancel the previous call if waiting for execution. Concurrent
       * calls to {@link mPackageInfoList} (in getInstalledPackages() or Collections.sort) cause
       * errors.
       */
      if (mUpdatePackagesFuture != null && !mUpdatePackagesFuture.isDone()) {
        if (mMySettings.isDebug()) {
          Util.debugLog(TAG, "updatePackagesList: cancelling previous call");
        }
        mUpdatePackagesFuture.cancel(true);
      }
      mUpdatePackagesFuture =
          mUpdatePackagesExecutor.submit(
              () -> updatePackagesListInBg(false, mMySettings.shouldDoQuickScan()));
    }
  }

  private boolean mIsUpdating;
  private final Object UPDATE_PKG_BG_LOCK = new Object();

  private boolean updatePackagesListInBg(boolean isBgDeepScan, boolean quickScan) {
    synchronized (UPDATE_PKG_BG_LOCK) {
      long startTime = System.currentTimeMillis();
      mIsUpdating = true;

      // Don't trouble Android on every call.
      if (System.currentTimeMillis() - mLastPackageManagerCall > 5000) {
        if (mMySettings.isDebug()) {
          Util.debugLog(TAG, "updatePackagesListInBg: updating packages list");
        }
        setProgress(PACKAGES_LIST, true, false, isBgDeepScan);

        mPackageInfoList =
            mPackageManager.getInstalledPackages(
                PackageManager.GET_PERMISSIONS | PM_GET_SIGNATURES);
        mLastPackageManagerCall = System.currentTimeMillis();
        mPkgParserFlavor.sortPkgList(mPackageInfoList);
      }

      /** if permissions database changes, manually call {@link #updatePermReferences()} */
      if (mPermRefList == null) {
        setProgress(REF_PERMS_LIST, true, false, isBgDeepScan);
        buildPermRefList();
      }

      if (!mMySettings.excludeAppOpsPerms() && mMySettings.canReadAppOps()) {
        setProgress(APP_OPS_LISTS, true, false, isBgDeepScan);
        if (mOpToSwitchList == null) {
          mOpToSwitchList = mAppOpsParser.buildOpToSwitchList();
        }
        if (mOpToDefModeList == null) {
          mOpToDefModeList = mAppOpsParser.buildOpToDefaultModeList();
        }
        if (mPermToOpCodeMap == null) {
          mPermToOpCodeMap = mAppOpsParser.buildPermissionToOpCodeMap();
        }
      }

      if (mMySettings.isDebug()) {
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
          if (mMySettings.isDebug()) {
            Util.debugLog(TAG, "updatePackagesListInBg: breaking loop, new call received");
          }
          return false;
        }

        if (isBgDeepScan && mMySettings.isSearching()) {
          isBgDeepScan = false;
          setProgress(mPackageInfoList.size(), true, false, false);
        }

        setProgress(i, false, false, isBgDeepScan);
        PackageInfo packageInfo = mPackageInfoList.get(i);
        if (mMySettings.isDebug()) {
          Util.debugLog(
              TAG, "updatePackagesListInBg: updating package: " + packageInfo.packageName);
        }

        Package pkg = new Package();
        if (isPkgUpdated(packageInfo, pkg, quickScan)) {
          packageList.add(pkg);
          mPkgParserFlavor.onPkgCreated(pkg);

          if (!isBgDeepScan && shouldDoRepeatUpdates()) {
            submitLiveData(packageList, false);
            mDoRepeatUpdates = false;
          }
        }
      }

      // finally update complete list and complete progress
      submitLiveData(packageList, true);
      setProgress(PKG_PROG_ENDS, false, true, isBgDeepScan);

      if (mMySettings.isDebug()) {
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

      mPkgParserFlavor.onPkgListCompleted();
      mIsUpdating = false;
      return true;
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// GETTERS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  // For RefCheckWorker
  @SuppressWarnings("UnusedDeclaration")
  public List<Package> getPackagesList(boolean update) {
    if (update) {
      if (!updatePackagesListInBg(false, false)) {
        return null; // Interrupted in between
      }
    }
    return mPackagesList;
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

  // when calling from PackageActivity for existing package
  public void updatePackage(Package pkg) {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "updatePackage: " + pkg.getLabel());
    }
    PackageInfo packageInfo = getPackageInfo(pkg.getName(), true);

    // package uninstalled, or disabled from MainActivity
    if (packageInfo == null || !isPkgUpdated(packageInfo, pkg, false)) {
      removePackage(pkg);
      return;
    }

    // update packages list when a Package's or Permission's state is changed so that RecyclerView
    // is updated on return to MainActivity
    Utils.runInFg(() -> mChangedPackage.setValue(pkg));

    // In case of search, also update temporary Package and Perms lists
    if (mMySettings.isSearching()) {
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
      if (mMySettings.isSearching()) {
        removeSearchPackage(pkg);
      } else {
        postLiveData(mPackagesList);
      }
    } else {
      Log.e(TAG, "removePackage: bad Package provided");
    }
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////// LIVE UPDATES ///////////////////////////
  //////////////////////////////////////////////////////////////////

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
    if (!mMySettings.isSearching()) {
      postLiveData(mPackagesList);
      if (mMySettings.isDebug()) {
        Util.debugLog(TAG, "submitLiveData: empty query text, returning");
      }
      return;
    }
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "submitLiveData: doing search");
    }
    handleSearchQuery(isFinal);
  }

  private void postLiveData(List<Package> packageList) {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "postLiveData: posting " + packageList.size() + " packages");
    }
    Utils.runInFg(() -> mPackagesListLive.setValue(new ArrayList<>(packageList)));
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
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "setProgress: value: " + value + ", isMax: " + isMax);
    }

    if (isMax) {
      Utils.runInFg(() -> mProgressMax.setValue(value));
      propertyChange.firePropertyChange(PROP_MAX_PROGRESS, 0, value);
      return;
    }

    if (isFinal) {
      Utils.runInFg(() -> mProgressNow.setValue(value));
      propertyChange.firePropertyChange(PROP_NOW_PROGRESS, 0, value);
      return;
    }

    // set progress updates, but not too frequent
    if ((System.currentTimeMillis() - mLastProgressTimeStamp) > 100) {
      Utils.runInFg(() -> mProgressNow.setValue(value));
      propertyChange.firePropertyChange(PROP_NOW_PROGRESS, 0, value);
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
      case PackageParser.PACKAGES_LIST:
        return R.string.creating_packages_list;
      case PackageParser.REF_PERMS_LIST:
        return R.string.reading_reference_perms;
      case PackageParser.APP_OPS_LISTS:
        return R.string.creating_app_ops_lists;
    }
    return 0;
  }

  //////////////////////////////////////////////////////////////////
  /////////////////////////// PACKAGES /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private boolean isPkgUpdated(PackageInfo packageInfo, Package pkg, boolean quickScan) {
    if (isFilteredOutPkgName(packageInfo.packageName)) {
      return false;
    }

    boolean isSystemApp = isSystemApp(packageInfo);
    if (isFilteredOutSystemPkg(isSystemApp)) {
      return false;
    }

    boolean isFrameworkApp = isFrameworkApp(packageInfo);
    if (isFilteredOutFrameworkPkg(isFrameworkApp)) {
      return false;
    }
    if (isFilteredOutUserPkg(isFrameworkApp, isSystemApp)) {
      return false;
    }

    ApplicationInfo appInfo = packageInfo.applicationInfo;

    boolean isEnabled = appInfo.enabled;
    if (isFilteredOutDisabledPkg(!isEnabled)) {
      return false;
    }

    if (isFilteredOutNoIconPkg(appInfo.icon == 0)) {
      return false;
    }

    List<Permission> permissionsList = new ArrayList<>();
    Boolean pkgIsReferenced = true;

    if (!quickScan) {
      if (mMySettings.isDebug()) {
        Util.debugLog(TAG, "isPkgUpdated: building permissions list");
      }
      permissionsList = getPermissionsList(packageInfo, pkg);

      // Exclude packages with no manifest permissions and no AppOps (excluding extra)
      if (isFilteredOutNoPermPkg(pkg)) {
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
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "isPkgUpdated: Package created");
    }

    return true;
  }

  private boolean isSystemApp(PackageInfo packageInfo) {
    return (packageInfo.applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0;
  }

  private boolean isFrameworkApp(PackageInfo packageInfo) {
    if (systemSignatures == null) {
      PackageInfo pkgInfo = getPackageInfo("android", false);
      if (pkgInfo != null) {
        systemSignatures = Arrays.asList(getPackageSignatures(pkgInfo));
      }
    }
    for (Signature signature : getPackageSignatures(packageInfo)) {
      if (systemSignatures.contains(signature)) {
        return true;
      }
    }
    return false;
  }

  private boolean isFilteredOutPkgName(String pkgName) {
    return mMySettings.isPkgExcluded(pkgName);
  }

  private boolean isFilteredOutSystemPkg(boolean isSystemPkg) {
    return mMySettings.excludeSystemApps() && isSystemPkg;
  }

  private boolean isFilteredOutFrameworkPkg(boolean isFrameworkPkg) {
    return mMySettings.excludeFrameworkApps() && isFrameworkPkg;
  }

  private boolean isFilteredOutUserPkg(boolean isFrameworkPkg, boolean isSystemPkg) {
    return mMySettings.excludeUserApps() && !isFrameworkPkg && !isSystemPkg;
  }

  private boolean isFilteredOutDisabledPkg(boolean isDisabledPkg) {
    return mMySettings.excludeDisabledApps() && isDisabledPkg;
  }

  private boolean isFilteredOutNoIconPkg(boolean isNoIconPkg) {
    return mMySettings.excludeNoIconApps() && isNoIconPkg;
  }

  private boolean isFilteredOutNoPermPkg(Package pkg) {
    return mMySettings.shouldExcludeNoPermApps()
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
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "buildPermRefList() called");
    }
    mPermRefList = new HashMap<>();
    for (PermissionEntity entity : mMySettings.getPermDb().getAll()) {
      mPermRefList.put(entity.pkgName + "_" + entity.permName, entity.state);
    }
  }

  private List<Permission> getPermissionsList(PackageInfo packageInfo, Package pkg) {
    String[] requestedPermissions = packageInfo.requestedPermissions;
    List<Permission> permissionsList = new ArrayList<>();

    Permission permission;
    int permCount = 0;
    int[] appOpsCount1 = new int[] {0, 0};
    List<Integer> processedAppOps = new ArrayList<>();

    if (requestedPermissions != null) {
      if (mMySettings.isDebug()) {
        Util.debugLog(TAG, "getPermissionsList: parsing permissions list");
      }
      for (int count = 0; count < requestedPermissions.length; count++) {
        String perm = requestedPermissions[count].replaceAll("\\s", "");
        permission = createPermission(packageInfo, perm, count);
        if (isNotFilteredOut(permission)) {
          permissionsList.add(permission);
          permCount++;
        }

        // not set AppOps corresponding to manifest permission
        if (!mMySettings.excludeAppOpsPerms() && mMySettings.canReadAppOps()) {
          int[] appOpsCount =
              createPermsAppOpsNotSet(packageInfo, perm, permissionsList, processedAppOps);
          appOpsCount1[0] += appOpsCount[0];
          appOpsCount1[1] += appOpsCount[1];
        }
      }
    }

    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "getPermissionsList: parsing AppOps");
    }

    int[] appOpsCount2 = new int[] {0, 0};
    int[] appOpsCount3 = new int[] {0, 0};
    if (!mMySettings.excludeAppOpsPerms() && mMySettings.canReadAppOps()) {
      if (mMySettings.isDebug()) {
        Util.debugLog(
            TAG, "getPermissionsList: parsing AppOps not corresponding to any manifest permission");
      }
      appOpsCount2 = createSetAppOps(packageInfo, permissionsList, processedAppOps);

      /*
        Do not count extra AppOps if app has no manifest permission and no other AppOp.
        Otherwise no apps will be excluded on excludeNoPermissionsApps() basis if even one extra
        AppOps is selected in list.
        In case of quick scan excludeNoPermissionsApps() is ignored, so show all.
      */
      if (!mMySettings.shouldExcludeNoPermApps()
          || requestedPermissions != null
          || appOpsCount2[0] != 0) {

        if (mMySettings.isDebug()) {
          Util.debugLog(TAG, "getPermissionsList: parsing extra AppOps");
        }

        // irrelevant / extra AppOps, not set and not corresponding to any manifest permission
        List<Integer> ops1 = new ArrayList<>();
        for (String opName : mMySettings.getExtraAppOps()) {
          int op = mMySettings.getAppOpsList().indexOf(opName);
          if (!processedAppOps.contains(op)) {
            ops1.add(op);
          }
        }

        if (ops1.size() != 0) {
          int[] ops2 = new int[ops1.size()];
          for (int i = 0; i < ops1.size(); i++) {
            ops2[i] = ops1.get(i);
          }
          appOpsCount3 = createExtraAppOps(packageInfo, permissionsList, ops2);
        }
      }
    }

    pkg.setTotalPermCount(requestedPermissions == null ? 0 : requestedPermissions.length);
    pkg.setPermCount(permCount);
    pkg.setTotalAppOpsCount(appOpsCount1[0] + appOpsCount2[0] + appOpsCount3[0]);
    pkg.setAppOpsCount(appOpsCount1[1] + appOpsCount2[1] + appOpsCount3[1]);

    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "getPermissionsList: permissions count: " + permissionsList.size());
    }

    return permissionsList;
  }

  private boolean isNotFilteredOut(Permission permission) {
    // always show extra AppOps except in search query
    if (permission.isExtraAppOp()) {
      return true;
    }

    if (mMySettings.isPermExcluded(permission.getName())) {
      return false;
    }
    if (mMySettings.excludeNotChangeablePerms() && !permission.isChangeable()) {
      return false;
    }
    if (mMySettings.excludeNotGrantedPerms() && !permission.isGranted()) {
      return false;
    }

    if (permission.isAppOps()) {
      return !mMySettings.excludeNotSetAppOps() || permission.isAppOpsSet();
    }

    if (mMySettings.excludePrivilegedPerms() && permission.isPrivileged()) {
      return false;
    }
    if (mMySettings.excludeSignaturePerms()
        && permission.getProtectionLevel().equals(Permission.PROTECTION_SIGNATURE)) {
      return false;
    }
    if (mMySettings.excludeDangerousPerms()
        && permission.getProtectionLevel().equals(Permission.PROTECTION_DANGEROUS)) {
      return false;
    }
    if (mMySettings.excludeNormalPerms()
        && permission.getProtectionLevel().equals(Permission.PROTECTION_NORMAL)) {
      return false;
    }
    return !mMySettings.excludeInvalidPermissions() || !permission.isProviderMissing();
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////// MANIFEST PERMISSION ///////////////////////
  //////////////////////////////////////////////////////////////////

  private Permission createPermission(PackageInfo packageInfo, String perm, int count) {
    int[] requestedPermissionsFlags = packageInfo.requestedPermissionsFlags;
    String protection = "Unknown";
    boolean isPrivileged = false;
    boolean isDevelopment = false;
    boolean isManifestPermAppOps = false;
    boolean isSystemFixed = false;
    boolean providerMissing = false;
    CharSequence permDesc = null;

    boolean isGranted =
        (requestedPermissionsFlags[count] & PackageInfo.REQUESTED_PERMISSION_GRANTED) != 0;

    try {
      PermissionInfo permissionInfo = mPackageManager.getPermissionInfo(perm, 0);

      int protectionLevel = permissionInfo.protectionLevel & PermissionInfo.PROTECTION_MASK_BASE;
      int protectionFlags = permissionInfo.protectionLevel & ~PermissionInfo.PROTECTION_MASK_BASE;
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
      } else {
        Log.e(
            TAG,
            "createPermission: protection level for "
                + permissionInfo.name
                + ": "
                + protectionLevel);
      }

      isPrivileged = (protectionFlags & PermissionInfo.PROTECTION_FLAG_PRIVILEGED) != 0;
      isDevelopment =
          protectionLevel == PermissionInfo.PROTECTION_SIGNATURE
              && (protectionFlags & PermissionInfo.PROTECTION_FLAG_DEVELOPMENT) != 0;
      isManifestPermAppOps = (protectionFlags & PermissionInfo.PROTECTION_FLAG_APPOP) != 0;

      permDesc = permissionInfo.loadDescription(mPackageManager);
    } catch (NameNotFoundException ignored) {
      // permissions provider is not available e.g. Play Services
      providerMissing = true;
    }

    String permState = isGranted ? Permission.GRANTED : Permission.REVOKED;
    RefPair refPair = getReference(packageInfo.packageName, perm, permState);
    GroupOrderPair groupOrderPair = mPermGroupsMapping.getOrderAndGroup(perm, false);

    boolean isSystemApp = isSystemApp(packageInfo);
    if (isSystemApp || isFrameworkApp(packageInfo)) {
      int permFlags = getPermissionFlags(perm, packageInfo.packageName);
      int systemFixedFlag = getSystemFixedFlag();
      if (permFlags >= 0 && systemFixedFlag >= 0) {
        isSystemFixed = (permFlags & systemFixedFlag) != 0;
      }
    }

    return new Permission(
        groupOrderPair.order,
        getIconResId(perm, groupOrderPair.group),
        false,
        false,
        false,
        -1,
        -1,
        null,
        false,
        packageInfo.packageName,
        perm,
        isGranted,
        protection,
        isPrivileged,
        isDevelopment,
        isManifestPermAppOps,
        isSystemFixed,
        providerMissing,
        refPair.isReferenced,
        refPair.reference,
        isSystemApp,
        permDesc);
  }

  private int getPermissionFlags(String perm, String pkg) {
    if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getPermissionFlags");
      return -1;
    } else {
      String command =
          Commands.GET_PERMISSION_FLAGS + " " + perm + " " + pkg + " " + Utils.getUserId();
      Object object = mPrivDaemonHandler.sendRequest(command);
      if (object instanceof Integer) {
        return (int) object;
      }
    }
    Log.e(TAG, "Error occurred in getPermissionFlags()");
    return -1;
  }

  private int SYSTEM_FIXED_FLAG = -1;

  private int getSystemFixedFlag() {
    if (SYSTEM_FIXED_FLAG != -1) {
      return SYSTEM_FIXED_FLAG;
    }

    try {
      SYSTEM_FIXED_FLAG = HiddenAPIs.getSystemFixedFlag();
      return SYSTEM_FIXED_FLAG;
    } catch (HiddenAPIsError e) {
      Log.e(TAG, "getSystemFixedFlag: " + e.toString());
    }

    if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getSystemFixedFlag");
      return SYSTEM_FIXED_FLAG;
    }

    Object object = mPrivDaemonHandler.sendRequest(Commands.GET_SYSTEM_FIXED_FLAG);
    if (object instanceof Integer) {
      SYSTEM_FIXED_FLAG = (int) object;
      return SYSTEM_FIXED_FLAG;
    }

    Log.e(TAG, "Error occurred in getSystemFixedFlag()");
    return SYSTEM_FIXED_FLAG;
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// APP OPS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private int[] createPermsAppOpsNotSet(
      PackageInfo packageInfo,
      String perm,
      List<Permission> permissionsList,
      List<Integer> processedAppOps) {

    Integer mappedOp = mPermToOpCodeMap.get(perm);
    if (mappedOp == null) {
      return new int[] {0, 0};
    }
    int op = mappedOp;

    int uid = packageInfo.applicationInfo.uid;

    List<MyPackageOps> pkgOpsList =
        mAppOpsParser.getOpsForPackage(uid, packageInfo.packageName, op);

    // do not return changed (set) ops, they are handled separately
    if (pkgOpsList != null && pkgOpsList.size() == 0) {
      return createAppOp(packageInfo, op, -1, permissionsList, processedAppOps, false, false, -1);
    }

    return new int[] {0, 0};
  }

  private int[] createSetAppOps(
      PackageInfo packageInfo, List<Permission> permissionsList, List<Integer> processedAppOps) {
    return createAppOpsList(packageInfo, permissionsList, processedAppOps, null);
  }

  private int[] createExtraAppOps(
      PackageInfo packageInfo, List<Permission> permissionsList, int[] ops) {
    return createAppOpsList(packageInfo, permissionsList, null, ops);
  }

  private int[] createAppOpsList(
      PackageInfo packageInfo,
      List<Permission> permissionsList,
      List<Integer> processedAppOps,
      int[] ops) {

    List<MyPackageOps> pkgOpsList = new ArrayList<>();
    int totalAppOpsCount = 0;
    int appOpsCount = 0;
    boolean isExtraAppOp = ops != null;
    int uid = packageInfo.applicationInfo.uid;

    List<MyPackageOps> list;
    if (isExtraAppOp) {
      for (int op : ops) {
        list = mAppOpsParser.getOpsForPackage(uid, packageInfo.packageName, op);
        if (list != null) {
          if (list.size() == 0) {
            int[] count =
                createAppOp(packageInfo, op, -1, permissionsList, processedAppOps, true, false, -1);
            totalAppOpsCount += count[0];
            appOpsCount += count[1];
          } else {
            pkgOpsList.addAll(list);
          }
        }
      }
    } else {
      list = mAppOpsParser.getOpsForPackage(uid, packageInfo.packageName, null);
      if (list != null) {
        pkgOpsList.addAll(list);
      }

      // UID mode: android-10.0.0_r1: AppOpsService.java#3378
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
        list = mAppOpsParser.getUidOps(uid);
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
                lastAccessTime);
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
      long accessTime) {
    int opSwitch = mOpToSwitchList.get(op);
    String dependsOn = op == opSwitch ? null : mMySettings.getAppOpsList().get(opSwitch);
    String opName = mMySettings.getAppOpsList().get(op);
    boolean isAppOpSet = true;
    if (opMode < 0) {
      isAppOpSet = false;
      opMode = mOpToDefModeList.get(op);
    }
    String opState = mMySettings.getAppOpsModes().get(opMode);
    RefPair refPair = getReference(packageInfo.packageName, opName, opState);
    GroupOrderPair groupOrderPair = mPermGroupsMapping.getOrderAndGroup(opName, true);

    Permission permission =
        new Permission(
            groupOrderPair.order,
            getIconResId(opName, groupOrderPair.group),
            true,
            isPerUid,
            isAppOpSet,
            opMode,
            accessTime,
            dependsOn,
            isExtraAppOp,
            packageInfo.packageName,
            opName,
            opMode != AppOpsManager.MODE_IGNORED && opMode != AppOpsManager.MODE_ERRORED,
            "AppOps",
            false,
            false,
            false,
            false,
            false,
            refPair.isReferenced,
            refPair.reference,
            isSystemApp(packageInfo),
            null);

    // so that it's not repeated
    if (!isExtraAppOp) {
      processedAppOps.add(op);
    }

    int appOpsCount = 0;

    if (isNotFilteredOut(permission)) {
      permissionsList.add(permission);
      appOpsCount = 1;
    } else if (!isExtraAppOp && mMySettings.isExtraAppOp(permission.getName())) {
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
  private PackageInfo getPackageInfo(String pkgName, boolean getPermissions) {
    int flags = PM_GET_SIGNATURES;
    if (getPermissions) {
      flags = PackageManager.GET_PERMISSIONS | flags;
    }
    try {
      return mPackageManager.getPackageInfo(pkgName, flags);
    } catch (NameNotFoundException e) {
      Log.e(TAG, "getPackageInfo: " + e.toString());
      return null;
    }
  }

  private Signature[] getPackageSignatures(PackageInfo packageInfo) {
    return packageInfo.signatures;
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

  public void handleSearchQuery(Boolean isFinal) {
    synchronized (mSearchQueryExecutor) {
      if (mSearchQueryFuture != null && !mSearchQueryFuture.isDone()) {
        if (isFinal != null && !isFinal) {
          return;
        }
        if (mMySettings.isDebug()) {
          Util.debugLog(TAG, "handleSearchQuery: cancelling previous call");
        }
        mSearchQueryFuture.cancel(true);
      }

      if (mMySettings.isSearching()) {
        mSearchQueryFuture = mSearchQueryExecutor.submit(() -> doSearchInBg(isFinal));
      } else {
        clearSearchLists();
        postLiveData(mPackagesList);
        if (mMySettings.isDebug()) {
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
      String queryText = mMySettings.getQueryText();

      for (int i = 0; i < origPkgList.size(); i++) {
        if (!mIsUpdating && Thread.interrupted()) {
          if (mMySettings.isDebug()) {
            Util.debugLog(TAG, "doSearchInBg: breaking loop, new call received");
          }
          return;
        }

        Package pkg = origPkgList.get(i);

        if (mMySettings.isDeepSearching()) {
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
          updateSearchLists(pkg, false);
        } else if (pkg.contains(queryText)) {
          synchronized (mSearchPkgList) {
            mSearchPkgList.add(pkg);
          }
        }
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
    List<Permission> permList = new ArrayList<>();
    int permCount = 0, appOpsCount = 0;
    for (Permission perm : pkg.getFullPermsList()) {
      if (perm.contains(mMySettings.getQueryText())) {
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
    } else if (removeOnly && mMySettings.isDeepSearching()) {
      removeSearchPackage(pkg);
    }
  }

  private void removeSearchPackage(Package pkg) {
    synchronized (mSearchPkgList) {
      if (mSearchPkgList.remove(pkg)) {
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

  //////////////////////////////////////////////////////////////////
  /////////////////////////// OBSERVABLE ///////////////////////////
  //////////////////////////////////////////////////////////////////

  private final PropertyChangeSupport propertyChange = new PropertyChangeSupport(this);
  public static final String PROP_MAX_PROGRESS = "PROP_MAX_PROGRESS";
  public static final String PROP_NOW_PROGRESS = "PROP_NOW_PROGRESS";

  @SuppressWarnings("UnusedDeclaration")
  public void addPropertyChangeListener(PropertyChangeListener listener) {
    propertyChange.addPropertyChangeListener(listener);
  }

  @SuppressWarnings("UnusedDeclaration")
  public void removePropertyChangeListener(PropertyChangeListener listener) {
    propertyChange.removePropertyChangeListener(listener);
  }
}
