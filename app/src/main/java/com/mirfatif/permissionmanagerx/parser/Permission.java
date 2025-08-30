package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.app.AppOpsManager;
import android.text.TextUtils;
import android.text.format.DateUtils;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.UserUtils;
import com.mirfatif.privtasks.Constants;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

public class Permission {

  public static final String PROTECTION_UNKNOWN = "PROTECTION_UNKNOWN";
  public static final String PROTECTION_DANGEROUS = "PROTECTION_DANGEROUS";
  public static final String PROTECTION_NORMAL = "PROTECTION_NORMAL";
  public static final String PROTECTION_SIGNATURE = "PROTECTION_SIGNATURE";
  public static final String PROTECTION_INTERNAL = "PROTECTION_INTERNAL";

  private static final String GRANTED = "GRANTED";
  private static final String REVOKED = "REVOKED";

  public static final String PROTECTION_APP_OPS = "APP_OPS";

  private final int mGroupId;
  private final int mIconResId;
  private final String mPackageName;
  private final String mPermissionName;
  private final boolean mGranted;
  private Boolean mReferenced;
  private String mReference;
  private final boolean mSystemApp, mFrameworkApp;

  private final String mProtectionLevel;
  private final boolean mPrivileged;
  private final boolean mDevelopment;
  private final boolean mManifestPermAppOp;
  private final boolean mSystemFixed, mPolicyFixed;
  private final String mProviderPkg;

  private final boolean mAppOp;
  private final boolean mPerUid;
  private final boolean mAppOpModeSet;
  private final boolean mUnknownAppOpMode;
  private final int mAppOpMode;
  private final long mAppOpAccessTime;
  private String mAppOpAccessTimeFormatted;
  private final String mDependsOn;
  private boolean mExtraAppOp;

  Permission(
      int groupId,
      int iconResId,
      String packageName,
      String name,
      boolean isGranted,
      boolean isSystemApp,
      boolean isFrameworkApp,
      String protectionLevel,
      boolean isPrivileged,
      boolean isDevelopment,
      boolean isManifestPermAppOp,
      boolean isSystemFixed,
      boolean isPolicyFixed,
      String providerPkg) {
    mGroupId = groupId;
    mIconResId = iconResId;
    mPackageName = packageName;
    mPermissionName = name;
    mGranted = isGranted;
    mSystemApp = isSystemApp;
    mFrameworkApp = isFrameworkApp;
    mProtectionLevel = protectionLevel;
    mPrivileged = isPrivileged;
    mDevelopment = isDevelopment;
    mManifestPermAppOp = isManifestPermAppOp;
    mSystemFixed = isSystemFixed;
    mPolicyFixed = isPolicyFixed;
    mProviderPkg = providerPkg;

    mAppOp = false;
    mPerUid = false;
    mAppOpModeSet = true;
    mUnknownAppOpMode = false;
    mAppOpMode = -1;
    mAppOpAccessTime = -1;
    mDependsOn = null;
    mExtraAppOp = false;
  }

  Permission(
      int groupId,
      Integer iconResId,
      String packageName,
      String name,
      boolean isGranted,
      boolean isSystemApp,
      boolean isFrameworkApp,
      boolean isPerUid,
      boolean opModeSet,
      boolean unknownOpMode,
      int appOpMode,
      long appOpAccessTime,
      String dependsOn,
      boolean isExtraAppOp) {
    mGroupId = groupId;
    mIconResId = iconResId;
    mPackageName = packageName;
    mPermissionName = name;
    mGranted = isGranted;
    mSystemApp = isSystemApp;
    mFrameworkApp = isFrameworkApp;
    mPerUid = isPerUid;
    mAppOpModeSet = opModeSet;
    mUnknownAppOpMode = unknownOpMode;
    mAppOpMode = appOpMode;
    mAppOpAccessTime = appOpAccessTime;
    mDependsOn = dependsOn;
    mExtraAppOp = isExtraAppOp;

    mAppOp = true;
    mProtectionLevel = PROTECTION_APP_OPS;
    mPrivileged = false;
    mDevelopment = false;
    mManifestPermAppOp = false;
    mSystemFixed = false;
    mPolicyFixed = false;
    mProviderPkg = null;
  }

  public int getGroupId() {
    return mGroupId;
  }

  public int getIconResId() {
    return mIconResId;
  }

  public boolean isAppOp() {
    return mAppOp;
  }

  public boolean isPerUid() {
    return mPerUid;
  }

  public boolean hasUnknownOpMode() {
    return mUnknownAppOpMode;
  }

  public boolean isAppOpModeSet() {
    return mAppOpModeSet;
  }

  public int getAppOpMode() {
    return mAppOpMode;
  }

  public String getAppOpAccessTime() {
    if (System.currentTimeMillis() - mAppOpAccessTime > TimeUnit.DAYS.toMillis(365)) {
      return null;
    }
    mAppOpAccessTimeFormatted =
        DateUtils.getRelativeTimeSpanString(
                mAppOpAccessTime, System.currentTimeMillis(), DateUtils.SECOND_IN_MILLIS)
            .toString();
    return mAppOpAccessTimeFormatted;
  }

  public boolean hasDependsOnPerm() {
    return mDependsOn != null;
  }

  public CharSequence getDependsOnName() {
    return mDependsOn;
  }

  public void setExtraAppOp() {
    mExtraAppOp = true;
  }

  public boolean isExtraAppOp() {
    return mExtraAppOp;
  }

  public String getName() {
    return mPermissionName;
  }

  public boolean isGranted() {
    return mGranted;
  }

  public String getProtectionLevel() {
    return mProtectionLevel;
  }

  public boolean isPrivileged() {
    return mPrivileged;
  }

  public boolean hasProviderPkg() {
    return mProviderPkg != null;
  }

  public void setReference(Boolean isReferenced, String reference) {
    mReferenced = isReferenced;
    mReference = reference;
  }

  public Boolean isReferenced() {
    return mReferenced;
  }

  public String getReference() {
    return mReference;
  }

  public boolean isCritical() {
    return (mSystemApp && mPrivileged) || mSystemFixed;
  }

  private Boolean mChangeable = null;

  public boolean isChangeable() {
    if (mChangeable == null) {
      mChangeable = isChangeableInternal();
    }
    return mChangeable;
  }

  private boolean isChangeableInternal() {
    if (mFrameworkApp || ExcFiltersData.INS.isCriticalApp(mPackageName)) {
      return false;
    }
    if (mAppOp) {
      return isAppOpPermChangeable(mDependsOn);
    } else {
      if ((!mProtectionLevel.equals(PROTECTION_DANGEROUS) && !mDevelopment)
          || (mSystemApp && mPrivileged)
          || mPolicyFixed) return false;
      return !mSystemFixed;
    }
  }

  public static boolean isAppOpPermChangeable(String dependsOn) {
    return dependsOn == null;
  }

  public boolean contains(String queryText, boolean caseSensitive) {
    if (!MySettings.INS.isSpecialSearch()) {
      return containsNot(queryText, caseSensitive);
    }

    boolean isEmpty = true;
    for (String str : queryText.split("\\|")) {
      if (TextUtils.isEmpty(str)) {
        continue;
      }
      isEmpty = false;
      if (containsAnd(str, caseSensitive)) {
        return true;
      }
    }
    return isEmpty;
  }

  private boolean containsAnd(String queryText, boolean caseSensitive) {
    for (String str : queryText.split("&")) {
      if (TextUtils.isEmpty(str)) {
        continue;
      }
      if (!containsNot(str, caseSensitive)) {
        return false;
      }
    }
    return true;
  }

  private boolean containsNot(String queryText, boolean caseSensitive) {
    boolean contains = true;
    if (MySettings.INS.isSpecialSearch() && queryText.startsWith("!")) {
      queryText = queryText.replaceAll("^!", "");
      contains = false;
    }

    caseSensitive = caseSensitive && MySettings.INS.isCaseSensitiveSearch();
    if (!caseSensitive) {
      queryText = queryText.toUpperCase();
    }

    for (String field :
        new String[] {
          getName(),
          ":" + getLocalizedProtectionLevel(mProtectionLevel),
          ((mAppOp || mManifestPermAppOp) ? SearchConstants.INS.SEARCH_APP_OPS : ""),
          ((mAppOp && mPerUid) ? SearchConstants.INS.SEARCH_UID : ""),
          (mPrivileged ? SearchConstants.INS.SEARCH_PRIVILEGED : ""),
          (mDevelopment ? SearchConstants.INS.SEARCH_DEV : ""),
          (mSystemFixed ? SearchConstants.INS.SEARCH_FIXED : ""),
          (mReferenced == null
              ? SearchConstants.INS.SEARCH_ORANGE
              : (mReferenced ? SearchConstants.INS.SEARCH_GREEN : SearchConstants.INS.SEARCH_RED)),
          getAppOpAccessTime() != null ? SearchConstants.INS.SEARCH_TIME : "",
          (mExtraAppOp ? SearchConstants.INS.SEARCH_EXTRA : "")
        }) {
      if (!caseSensitive) {
        field = field.toUpperCase();
      }
      if (field.contains(queryText)) {
        return contains;
      }
    }
    return !contains;
  }

  public boolean areContentsTheSame(Permission newPerm) {
    if (!Objects.equals(isReferenced(), newPerm.isReferenced())) {
      return false;
    }

    if (!isAppOp()) {
      return isGranted() == newPerm.isGranted();
    }

    if (getAppOpMode() != newPerm.getAppOpMode()) {
      return false;
    }

    if (hasUnknownOpMode() != newPerm.hasUnknownOpMode()) {
      return false;
    }

    if (isAppOpModeSet() != newPerm.isAppOpModeSet()) {
      return false;
    }

    if (!Objects.equals(mAppOpAccessTimeFormatted, newPerm.getAppOpAccessTime())) {
      return false;
    }

    return isExtraAppOp() == newPerm.isExtraAppOp();
  }

  public boolean isSamePerm(Permission newPerm) {
    return isAppOp() == newPerm.isAppOp()
        && getName().equals(newPerm.getName())
        && (!isAppOp() || isPerUid() == newPerm.isPerUid());
  }

  public void setAppOpMode(Package pkg, int mode) {
    if (isAppOp()) {
      Integer op = AppOpsParser.INS.getAppOpCode(getName());
      if (op != null) {
        DaemonIface.INS.setAppOpMode(pkg.getUid(), isPerUid() ? null : pkg.getName(), op, mode);
      } else {
        UiUtils.showToast(R.string.something_went_wrong);
      }
    }
  }

  public void toggleState(Package pkg) {
    if (!isAppOp()) {
      DaemonIface.INS.setPermState(
          !isGranted(), pkg.getName(), getName(), UserUtils.getUserId(pkg.getUid()));
    }
  }

  public String createRefStringForDb() {
    return isAppOp() ? createRefStringForDb(getAppOpMode()) : createRefStringForDb(isGranted());
  }

  public static String createRefStringForDb(boolean granted) {
    return granted ? Permission.GRANTED : Permission.REVOKED;
  }

  public static String createRefStringForDb(int appOpMode) {
    return AppOpsParser.INS.opModeToName(appOpMode);
  }

  public static boolean isAppOpGranted(int appOpMode) {
    return appOpMode >= 0
        && appOpMode != AppOpsManager.MODE_IGNORED
        && appOpMode != AppOpsManager.MODE_ERRORED;
  }

  public static int getAppOpMode(boolean granted) {
    return granted ? AppOpsManager.MODE_ALLOWED : AppOpsManager.MODE_IGNORED;
  }

  public static Boolean isReferenced(String dbRefString, boolean granted) {
    return dbRefString == null ? null : dbRefString.equals(createRefStringForDb(granted));
  }

  public static Boolean isReferenced(String dbRefString, int appOpMode) {
    return dbRefString == null ? null : dbRefString.equals(createRefStringForDb(appOpMode));
  }

  public CharSequence getPermNameString() {
    CharSequence permName = getName();
    if (mAppOp && hasDependsOnPerm()) {
      permName = TextUtils.concat(permName, " (", getDependsOnName(), ")");
    }
    return permName;
  }

  private static String getLocalizedProtectionLevel(String protectionLevel) {
    return switch (protectionLevel) {
      case PROTECTION_NORMAL -> SearchConstants.INS.SEARCH_PROT_NORMAL;
      case PROTECTION_DANGEROUS -> SearchConstants.INS.SEARCH_PROT_DANGEROUS;
      case PROTECTION_SIGNATURE -> SearchConstants.INS.SEARCH_PROT_SIGNATURE;
      case PROTECTION_INTERNAL -> SearchConstants.INS.SEARCH_PROT_INTERNAL;
      case PROTECTION_APP_OPS -> SearchConstants.INS.SEARCH_APP_OPS;
      default -> SearchConstants.INS.SEARCH_PROT_UNKNOWN;
    };
  }

  public static String getLocalizedProtLevelString(
      boolean isAppOp,
      String protection,
      boolean privileged,
      boolean development,
      boolean manifestPermAppOp,
      boolean perUid,
      boolean extraAppOp,
      boolean systemPolicyFixed) {
    String protectionLevel = getLocalizedProtectionLevel(protection).replaceFirst("^:", "");

    if (isAppOp) {
      if (perUid) {
        protectionLevel = getString(R.string.prot_lvl_uid_mode, protectionLevel);
      }
      if (extraAppOp) {
        protectionLevel = getString(R.string.prot_lvl_extra, protectionLevel);
      }
    } else {
      if (development) {
        protectionLevel = getString(R.string.prot_lvl_development2, protectionLevel);
      }
      if (manifestPermAppOp) {
        protectionLevel = getString(R.string.prot_lvl_app_ops2, protectionLevel);
      }
      if (privileged) {
        protectionLevel = getString(R.string.prot_lvl_privileged2, protectionLevel);
      }
      if (systemPolicyFixed) {
        protectionLevel = getString(R.string.prot_lvl_fixed2, protectionLevel);
      }
    }
    return protectionLevel;
  }

  public String getLocalizedProtLevelString() {
    return getLocalizedProtLevelString(
        isAppOp(),
        getProtectionLevel(),
        isPrivileged(),
        mDevelopment,
        mManifestPermAppOp,
        mPerUid,
        mExtraAppOp,
        mSystemFixed || mPolicyFixed);
  }

  public String getLocalizedPermStateName() {
    if (isAppOp()) {
      return getLocalizedAppOpModeName(AppOpsParser.INS.opModeToName(getAppOpMode()));
    } else {
      return getLocalizedPermStateName(isGranted());
    }
  }

  public static String getLocalizedAppOpModeName(String appOpModeName) {
    if (appOpModeName == null) {
      return null;
    }
    return switch (appOpModeName) {
      case Constants.APP_OP_MODE_ALLOW -> getString(R.string.app_op_mode_allow);
      case Constants.APP_OP_MODE_IGNORE -> getString(R.string.app_op_mode_ignore);
      case Constants.APP_OP_MODE_DENY -> getString(R.string.app_op_mode_deny);
      case Constants.APP_OP_MODE_DEFAULT -> getString(R.string.app_op_mode_default);
      case Constants.APP_OP_MODE_FG -> getString(R.string.app_op_mode_foreground);
      default -> appOpModeName;
    };
  }

  public static String getLocalizedPermStateName(boolean granted) {
    return granted ? getString(R.string.perm_mode_granted) : getString(R.string.perm_mode_revoked);
  }

  public static void getLocalizedPermStateNames(
      List<CharSequence> names,
      List<Boolean> states,
      AtomicInteger preCheckedIndex,
      boolean granted) {
    for (boolean b : new Boolean[] {true, false}) {
      names.add(getLocalizedPermStateName(b));
      states.add(b);
    }

    preCheckedIndex.set(granted ? 0 : 1);
  }

  public static void getLocalizedAppOpModeNames(
      List<CharSequence> names,
      List<Integer> modes,
      AtomicInteger preCheckedIndex,
      int appOpMode,
      String appOpName) {
    boolean noFg =
        appOpName.equals("RUN_IN_BACKGROUND") || appOpName.equals("RUN_ANY_IN_BACKGROUND");

    int modeCount = AppOpsParser.INS.getAppOpModeCount();

    for (int mode = 0; mode < modeCount; mode++) {
      String modeName = AppOpsParser.INS.opModeToName(mode);
      if (modeName == null || (noFg && Constants.APP_OP_MODE_FG.equals(modeName))) {
        continue;
      }

      names.add(getLocalizedAppOpModeName(modeName));
      modes.add(mode);
    }

    preCheckedIndex.set(modes.indexOf(appOpMode));
  }
}
