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
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.PermGroupsMapping.GroupOrderPair;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.MyPackageOps;
import com.mirfatif.privtasks.Util;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;

public class PackageParser {

  private static final String TAG = "PackageParser";

  private static PackageParser mPackageParser;
  private final PackageManager mPackageManager;
  private final MySettings mMySettings;
  private final AppOpsParser mAppOpsParser;
  private final PrivDaemonHandler mPrivDaemonHandler;
  private final PermGroupsMapping mPermGroupsMapping;

  private final MutableLiveData<List<Package>> mPackagesListLive = new MutableLiveData<>();
  private final MutableLiveData<Package> mChangedPackage = new MutableLiveData<>();
  private final MutableLiveData<Integer> mProgressMax = new MutableLiveData<>();
  private final MutableLiveData<Integer> mProgressNow = new MutableLiveData<>();

  private List<PackageInfo> packageInfoList;
  private final List<Package> mPackagesList = new ArrayList<>();
  private List<Signature> systemSignatures;
  private final Map<String, Integer> mPermIconsResIds = new HashMap<>();
  private List<Integer> mOpToSwitchList;
  private List<Integer> mOpToDefModeList;
  private Map<String, String> mPermRefList;
  private Map<String, Integer> mPermToOpCodeMap;

  @SuppressWarnings("deprecation")
  private static final int PM_GET_SIGNATURES = PackageManager.GET_SIGNATURES;

  // create singleton instance of PackageParser so that all activities can update mPackagesListLive
  // whenever needed
  public static synchronized PackageParser getInstance() {
    if (mPackageParser == null) {
      mPackageParser = new PackageParser();
    }
    return mPackageParser;
  }

  private PackageParser() {
    mPackageManager = App.getContext().getPackageManager();
    mMySettings = MySettings.getInstance();
    mAppOpsParser = new AppOpsParser();
    mPrivDaemonHandler = PrivDaemonHandler.getInstance();
    mPermGroupsMapping = new PermGroupsMapping();
  }

  @SuppressLint("PackageManagerGetSignatures")
  private PackageInfo getPackageInfo(String pkgName, boolean getPermissions) {
    int flags = PM_GET_SIGNATURES;
    if (getPermissions) {
      flags = PackageManager.GET_PERMISSIONS | flags;
    }
    try {
      return mPackageManager.getPackageInfo(pkgName, flags);
    } catch (NameNotFoundException e) {
      Log.e(TAG, e.toString());
      return null;
    }
  }

  @SuppressWarnings("deprecation")
  private Signature[] getPackageSignatures(PackageInfo packageInfo) {
    return packageInfo.signatures;
  }

  public LiveData<List<Package>> getPackagesListLive() {
    updatePackagesList(true); // update list on app (re)launch
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

  public Package getPackage(int position) {
    synchronized (mPackagesList) {
      if (position < 0 || position >= mPackagesList.size()) {
        Log.e("PackageParser", "getPackage(): bad position: " + position);
        return null;
      }
      return mPackagesList.get(position);
    }
  }

  public int getPackagePosition(Package pkg) {
    synchronized (mPackagesList) {
      int position = mPackagesList.indexOf(pkg);
      if (position == -1) {
        Log.e("PackageParser", "getPackagePosition(): bad Package provided");
        return -1;
      }
      return position;
    }
  }

  public void removePackage(Package pkg) {
    if (mIsUpdating) {
      updatePackagesList(true);
      return;
    }
    boolean res;
    synchronized (mPackagesList) {
      res = mPackagesList.remove(pkg);
    }
    if (res) {
      submitLiveData(mPackagesList);
    } else {
      Log.e("PackageParser", "removePackage(): bad Package provided");
    }
  }

  public int getPackagesListSize() {
    return mPackagesList.size();
  }

  private long lastPackageManagerCall = 0;
  private long mUpdatePackageListRefId;
  private Future<?> updatePackagesFuture;

  public void updatePackagesList(boolean doRepeatUpdates) {
    if (mMySettings.isDebug()) {
      Util.debugLog("updatePackagesList", "doRepeatUpdates: " + doRepeatUpdates);
    }
    long myId = mUpdatePackageListRefId = System.nanoTime(); // to handle concurrent calls

    /**
     * In case of multiple calls, cancel the previous call if waiting for execution. Concurrent
     * calls to {@link packageInfoList} (in getInstalledPackages() or Collections.sort) cause
     * errors. Use {@link myId} and {@link mUpdatePackageListRefId} to break the previous loop if
     * new call comes.
     */
    if (updatePackagesFuture != null && !updatePackagesFuture.isDone()) {
      if (mMySettings.isDebug()) {
        Util.debugLog("updatePackagesList", "Cancelling previous call");
      }
      updatePackagesFuture.cancel(false);
    }
    updatePackagesFuture =
        Utils.updatePackagesExecutor(() -> updatePackagesListInBg(doRepeatUpdates, myId));
  }

  // For RefCheckWorker
  @SuppressWarnings("UnusedDeclaration")
  public List<Package> getPackagesList(boolean update) {
    if (update) {
      long myId = mUpdatePackageListRefId = System.nanoTime();
      updatePackagesListInBg(false, myId);
      if (myId != mUpdatePackageListRefId) {
        return null; // Interrupted in between
      }
    }
    return mPackagesList;
  }

  private boolean mIsUpdating;

  private synchronized void updatePackagesListInBg(boolean doRepeatUpdates, long myId) {
    long startTime = System.currentTimeMillis();
    mIsUpdating = true;

    // Don't trouble Android on every call.
    if (System.currentTimeMillis() - lastPackageManagerCall > 5000) {
      if (mMySettings.isDebug()) {
        Util.debugLog("updatePackagesListInBg", "Updating packages list");
      }
      setProgress(CREATE_PACKAGES_LIST, true, false);

      packageInfoList =
          mPackageManager.getInstalledPackages(PackageManager.GET_PERMISSIONS | PM_GET_SIGNATURES);
      lastPackageManagerCall = System.currentTimeMillis();

      packageInfoList.sort(
          (o1, o2) -> {
            String p1 = o1.applicationInfo.loadLabel(mPackageManager).toString().toUpperCase();
            String p2 = o2.applicationInfo.loadLabel(mPackageManager).toString().toUpperCase();
            return p1.compareTo(p2);
          });
    }

    /** if permissions database changes, manually call {@link #updatePermReferences()} */
    if (mPermRefList == null) {
      setProgress(REF_PERMS_LIST, true, false);
      buildPermRefList();
    }

    if (!mMySettings.excludeAppOpsPerms() && mMySettings.canReadAppOps()) {
      if (mOpToSwitchList == null) {
        setProgress(OP_TO_SWITCH_LIST, true, false);
        mOpToSwitchList = mAppOpsParser.buildOpToSwitchList();
      }
      if (mOpToDefModeList == null) {
        setProgress(OP_TO_DEF_MODE_LIST, true, false);
        mOpToDefModeList = mAppOpsParser.buildOpToDefaultModeList();
      }
      if (mPermToOpCodeMap == null) {
        setProgress(PERM_TO_OP_CODE_MAP, true, false);
        mPermToOpCodeMap = mAppOpsParser.buildPermissionToOpCodeMap();
      }
    }

    if (mMySettings.isDebug()) {
      Util.debugLog("updatePackagesListInBg", "Total packages count: " + packageInfoList.size());
    }
    // set progress bar scale ASAP
    setProgress(packageInfoList.size(), true, false);

    // using global mPackagesList here might give wrong results in case of concurrent calls
    List<Package> packageList = new ArrayList<>();

    for (int i = 0; i < packageInfoList.size(); i++) {
      // handle concurrent calls
      if (myId != mUpdatePackageListRefId) {
        if (mMySettings.isDebug()) {
          Util.debugLog("updatePackagesListInBg", "Breaking loop, new call received");
        }
        return;
      }

      setProgress(i, false, false);
      PackageInfo packageInfo = packageInfoList.get(i);
      if (mMySettings.isDebug()) {
        Util.debugLog("updatePackagesListInBg", "Updating package: " + packageInfo.packageName);
      }

      Package pkg = new Package();
      if (isPkgUpdated(packageInfo, pkg)) {
        packageList.add(pkg);
      }

      if (mMySettings.shouldDoRepeatUpdates() && doRepeatUpdates) {
        if (shouldUpdateLiveData()) {
          submitLiveData(packageList);
        }
      }
    }

    // finally update complete list and complete progress
    submitLiveData(packageList);
    setProgress(packageInfoList.size(), false, true);

    if (mMySettings.isDebug()) {
      Util.debugLog(
          "updatePackagesListInBg",
          "Total time: " + (System.currentTimeMillis() - startTime) + "ms");
    }

    mIsUpdating = false;
  }

  //////////////////////////////////////////////////////////////////
  /////////////////////////// PROGRESS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private long mLastProgressTimeStamp = 0;

  private void setProgress(int value, boolean isMax, boolean isFinal) {
    if (mMySettings.isDebug()) {
      Util.debugLog("setProgress", "Value: " + value + ", isMax: " + isMax);
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
    if ((System.currentTimeMillis() - mLastProgressTimeStamp) > 50) {
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

  // to show progress
  private static final int CREATE_PACKAGES_LIST = -1;
  private static final int REF_PERMS_LIST = -2;
  private static final int OP_TO_SWITCH_LIST = -3;
  private static final int OP_TO_DEF_MODE_LIST = -4;
  private static final int PERM_TO_OP_CODE_MAP = -5;

  public int getProgressTextResId(int progressMax) {
    switch (progressMax) {
      case PackageParser.CREATE_PACKAGES_LIST:
        return R.string.creating_packages_list;
      case PackageParser.REF_PERMS_LIST:
        return R.string.reading_reference_perms;
      case PackageParser.OP_TO_SWITCH_LIST:
        return R.string.mapping_op_to_switch;
      case PackageParser.OP_TO_DEF_MODE_LIST:
        return R.string.listing_op_default_modes;
      case PackageParser.PERM_TO_OP_CODE_MAP:
        return R.string.mapping_perms_to_ops;
    }
    return 0;
  }

  //////////////////////////////////////////////////////////////////
  /////////////////////////// PACKAGES /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private boolean isPkgUpdated(PackageInfo packageInfo, Package pkg) {
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

    if (mMySettings.isDebug()) {
      Util.debugLog("PackageParser", "isPkgUpdated(): building permissions list");
    }
    List<Permission> permissionsList = getPermissionsList(packageInfo, pkg);

    // Exclude packages with no manifest permissions and no AppOps (excluding extra)
    if (isFilteredOutNoPermPkg(pkg)) {
      return false;
    }

    Boolean pkgIsReferenced = true;
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

    // Update the package even if it's being filtered out due to permissions. It's because if this
    // method is being called from within PackageActivity, permissions list must be updated so that
    // the change is visible to the user in PackageActivity.
    pkg.updatePackage(
        appInfo.loadLabel(mPackageManager).toString(),
        packageInfo.packageName,
        permissionsList,
        isFrameworkApp,
        isSystemApp,
        isEnabled,
        appInfo.uid,
        pkgIsReferenced);
    if (mMySettings.isDebug()) {
      Util.debugLog("PackageParser", "isPkgUpdated(): Package created");
    }

    // If package has some permissions but currently if we are in deep (permission) search and
    // all permissions are filtered out, package should also be filtered out.
    return !mMySettings.isSearching()
        || !mMySettings.isDeepSearchEnabled()
        || pkg.getPermCount() != 0
        || pkg.getAppOpsCount() != 0;
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

  // when calling from PackageActivity for existing package
  public void updatePackage(Package pkg) {
    if (mMySettings.isDebug()) {
      Util.debugLog("PackageParser", "updatePackage(): " + pkg.getLabel());
    }
    PackageInfo packageInfo = getPackageInfo(pkg.getName(), true);

    // package uninstalled, or disabled from MainActivity, or ref state changed during deep search
    if (packageInfo == null || !isPkgUpdated(packageInfo, pkg)) {
      removePackage(pkg);
      return;
    }

    // update packages list when a Package's or Permission's state is changed so that RecyclerView
    // is updated on return to MainActivity
    submitLiveData(mPackagesList);
    Utils.runInFg(() -> mChangedPackage.setValue(pkg));
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
    return mMySettings.excludeNoPermissionsApps()
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
      Util.debugLog("updatePackagesListInBg", "buildPermRefList() called");
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
        Util.debugLog("PackageParser", "getPermissionsList(): Parsing permissions list");
      }
      for (int count = 0; count < requestedPermissions.length; count++) {
        String perm = requestedPermissions[count];
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
      Util.debugLog("PackageParser", "getPermissionsList(): Parsing AppOps");
    }

    int[] appOpsCount2 = new int[] {0, 0};
    int[] appOpsCount3 = new int[] {0, 0};
    if (!mMySettings.excludeAppOpsPerms() && mMySettings.canReadAppOps()) {
      if (mMySettings.isDebug()) {
        Util.debugLog(
            "PackageParser",
            "getPermissionsList(): Parsing AppOps not corresponding to any manifest permission");
      }
      appOpsCount2 = createSetAppOps(packageInfo, permissionsList, processedAppOps);

      // Do not count extra AppOps if app has no manifest permission and no other AppOp.
      // Otherwise no apps will be excluded on excludeNoPermissionsApps() basis if even one extra
      // AppOps is selected in list.
      if (!mMySettings.excludeNoPermissionsApps()
          || requestedPermissions != null
          || appOpsCount2[0] != 0) {

        if (mMySettings.isDebug()) {
          Util.debugLog("PackageParser", "getPermissionsList(): Parsing extra AppOps");
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
      Util.debugLog(
          "PackageParser", "getPermissionsList(): Permissions count: " + permissionsList.size());
    }

    return permissionsList;
  }

  private boolean isNotFilteredOut(Permission permission) {
    if (mMySettings.isSearching()
        && mMySettings.isDeepSearchEnabled()
        && !permission.contains(mMySettings.getQueryText())) {
      return false;
    }

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

      @SuppressWarnings("deprecation")
      int protectionLevel = permissionInfo.protectionLevel & PermissionInfo.PROTECTION_MASK_BASE;
      @SuppressWarnings("deprecation")
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
        Log.e(TAG, "Protection level for " + permissionInfo.name + ": " + protectionLevel);
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

  private int systemFixedFlag = -1;

  private int getSystemFixedFlag() {
    if (systemFixedFlag != -1) {
      return systemFixedFlag;
    }

    if (mMySettings.canUseHiddenAPIs()) {
      // hidden API
      int flag = Utils.getIntField("FLAG_PERMISSION_SYSTEM_FIXED", PackageManager.class, TAG);
      if (flag != Utils.INT_FIELD_ERROR) {
        systemFixedFlag = flag;
        return systemFixedFlag;
      }
      Utils.hiddenAPIsNotWorking(TAG, "Could not get FLAG_PERMISSION_SYSTEM_FIXED field");
    } else if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getSystemFixedFlag");
      return -1;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(Commands.GET_SYSTEM_FIXED_FLAG);
      if (object instanceof Integer) {
        systemFixedFlag = (int) object;
        return systemFixedFlag;
      }
    }
    Log.e(TAG, "Error occurred in getSystemFixedFlag()");
    return -1;
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

  private int getIconResId(String perm, String group) {
    Integer iconResId = mPermIconsResIds.get(perm);
    if (iconResId == null) {
      iconResId = Utils.getIntField("g_" + group.toLowerCase(), R.drawable.class, TAG);
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

  public List<String> buildAppOpsList() {
    return mAppOpsParser.buildAppOpsList();
  }

  public List<String> buildAppOpsModes() {
    return mAppOpsParser.buildAppOpsModes();
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// SEARCH /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private void submitLiveData(List<Package> packagesList) {
    synchronized (mPackagesList) {
      List<Package> newList = new ArrayList<>(packagesList);
      mPackagesList.clear();
      mPackagesList.addAll(newList);
    }
    if (mMySettings.isDeepSearchEnabled()) {
      Utils.runInFg(() -> mPackagesListLive.setValue(mPackagesList));
      if (mMySettings.isDebug()) {
        Util.debugLog(
            "submitLiveData",
            "Shallow search disabled, posting " + mPackagesList.size() + " packages");
      }
      return;
    }
    if (mMySettings.isDebug()) {
      Util.debugLog("submitLiveData", "Doing shallow search");
    }
    handleSearchQuery(false);
  }

  private long mHandleSearchQueryRefId;
  private Future<?> searchQueryFuture;

  public void handleSearchQuery(boolean doRepeatUpdates) {
    if (!mMySettings.isSearching()) {
      Utils.runInFg(() -> mPackagesListLive.setValue(mPackagesList));
      if (mMySettings.isDebug()) {
        Util.debugLog(
            "handleSearchQuery", "Empty query text, posting " + mPackagesList.size() + " packages");
      }
      return;
    }

    long myId = mHandleSearchQueryRefId = System.nanoTime();
    if (searchQueryFuture != null && !searchQueryFuture.isDone()) {
      if (mMySettings.isDebug()) {
        Util.debugLog("handleSearchQuery", "Cancelling previous call");
      }
      searchQueryFuture.cancel(false);
    }
    searchQueryFuture = Utils.searchQueryExecutor(() -> doSearchInBg(doRepeatUpdates, myId));
  }

  private void doSearchInBg(boolean doRepeatUpdates, long myId) {
    String queryText = mMySettings.getQueryText();
    List<Package> packageList = new ArrayList<>();

    synchronized (mPackagesList) {
      for (Package pkg : new ArrayList<>(mPackagesList)) {
        if (myId != mHandleSearchQueryRefId) {
          if (mMySettings.isDebug()) {
            Util.debugLog("doSearchInBg", "Breaking loop, new call received");
          }
          return;
        }
        if (pkg.contains(queryText)) {
          packageList.add(pkg);
        }
        if (doRepeatUpdates && shouldUpdateLiveData()) {
          postLiveData(packageList);
          packagesListUpdateTimeStamp = System.currentTimeMillis();
        }
      }
    }
    postLiveData(packageList);
  }

  private void postLiveData(List<Package> packageList) {
    if (mMySettings.isDebug()) {
      Util.debugLog("handleSearchQuery", "Posting " + packageList.size() + " packages");
    }
    Utils.runInFg(() -> mPackagesListLive.setValue(packageList));
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
