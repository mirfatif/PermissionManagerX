package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.text.TextUtils;
import android.text.format.DateUtils;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.MySettingsFlavor;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.util.UserUtils;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class Permission {

  public static final String PROTECTION_UNKNOWN = "PROTECTION_UNKNOWN";
  public static final String PROTECTION_DANGEROUS = "PROTECTION_DANGEROUS";
  public static final String PROTECTION_NORMAL = "PROTECTION_NORMAL";
  public static final String PROTECTION_SIGNATURE = "PROTECTION_SIGNATURE";
  public static final String PROTECTION_INTERNAL = "PROTECTION_INTERNAL";

  public static final String GRANTED = "GRANTED";
  public static final String REVOKED = "REVOKED";

  private static final String APP_OPS = "APP_OPS";

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
    mProtectionLevel = APP_OPS;
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
    return PkgParserFlavor.INS.getPermName(
        mDependsOn, PermGroupsMapping.INS.getGroupId(mDependsOn, true));
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

  public String getProviderPkg() {
    return mProviderPkg;
  }

  public boolean isSystemFixed() {
    return mSystemFixed;
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
    boolean allowCriticChanges = MySettingsFlavor.INS.allowCriticalChanges();
    if ((mFrameworkApp && !allowCriticChanges) || ExcFiltersData.INS.isCriticalApp(mPackageName)) {
      return false;
    }
    if (mAppOp) {
      return !hasDependsOnPerm();
    } else {

      return (mProtectionLevel.equals(PROTECTION_DANGEROUS) || mDevelopment)
          && (!mSystemApp || !mPrivileged || allowCriticChanges)
          && !mPolicyFixed
          && (!mSystemFixed || (allowCriticChanges && DaemonHandler.INS.isSystemUid()));
    }
  }

  public boolean isChangeableForDump() {
    if (mAppOp) {
      return !hasDependsOnPerm();
    }

    return (mProtectionLevel.equals(PROTECTION_DANGEROUS) || mDevelopment)
        && !mPolicyFixed
        && !mSystemFixed;
  }

  public CharSequence getPermNameString() {
    CharSequence permName = PkgParserFlavor.INS.getPermName(this);
    if (mAppOp && hasDependsOnPerm()) {
      permName = TextUtils.concat(permName, " (", getDependsOnName(), ")");
    }
    return permName;
  }

  private String getLocalizedProtectionLevel() {
    switch (mProtectionLevel) {
      case PROTECTION_UNKNOWN:
      default:
        return SearchConstants.INS.SEARCH_PROT_UNKNOWN;
      case PROTECTION_NORMAL:
        return SearchConstants.INS.SEARCH_PROT_NORMAL;
      case PROTECTION_DANGEROUS:
        return SearchConstants.INS.SEARCH_PROT_DANGEROUS;
      case PROTECTION_SIGNATURE:
        return SearchConstants.INS.SEARCH_PROT_SIGNATURE;
      case PROTECTION_INTERNAL:
        return SearchConstants.INS.SEARCH_PROT_INTERNAL;
      case APP_OPS:
        return SearchConstants.INS.SEARCH_APP_OPS;
    }
  }

  public String getProtLevelString() {
    String protectionLevel = getLocalizedProtectionLevel().replaceFirst("^:", "");

    if (mAppOp) {
      if (mPerUid) {
        protectionLevel = getString(R.string.prot_lvl_uid_mode, protectionLevel);
      }
      if (mExtraAppOp) {
        protectionLevel = getString(R.string.prot_lvl_extra, protectionLevel);
      }
    } else {
      if (mDevelopment) {
        protectionLevel = getString(R.string.prot_lvl_development2, protectionLevel);
      }
      if (mManifestPermAppOp) {
        protectionLevel = getString(R.string.prot_lvl_app_ops2, protectionLevel);
      }
      if (mPrivileged) {
        protectionLevel = getString(R.string.prot_lvl_privileged2, protectionLevel);
      }
      if (mSystemFixed || mPolicyFixed) {
        protectionLevel = getString(R.string.prot_lvl_fixed2, protectionLevel);
      }
    }
    return protectionLevel;
  }

  public boolean contains(Package pkg, String queryText, boolean caseSensitive) {
    if (!MySettings.INS.isSpecialSearch()) {
      return containsNot(pkg, queryText, caseSensitive);
    }

    boolean isEmpty = true;
    for (String str : queryText.split("\\|")) {
      if (TextUtils.isEmpty(str)) {
        continue;
      }
      isEmpty = false;
      if (containsAnd(pkg, str, caseSensitive)) {
        return true;
      }
    }
    return isEmpty;
  }

  private boolean containsAnd(Package pkg, String queryText, boolean caseSensitive) {
    for (String str : queryText.split("&")) {
      if (TextUtils.isEmpty(str)) {
        continue;
      }
      if (!containsNot(pkg, str, caseSensitive)) {
        return false;
      }
    }
    return true;
  }

  private boolean containsNot(Package pkg, String queryText, boolean caseSensitive) {
    boolean contains = true;
    if (MySettings.INS.isSpecialSearch() && queryText.startsWith("!")) {
      queryText = queryText.replaceAll("^!", "");
      contains = false;
    }

    Boolean handled = MySettingsFlavor.INS.handleSearchQuery(queryText, pkg, this);
    if (Boolean.TRUE.equals(handled)) {
      return contains;
    } else if (Boolean.FALSE.equals(handled)) {
      return !contains;
    }

    caseSensitive = caseSensitive && MySettings.INS.isCaseSensitiveSearch();
    if (!caseSensitive) {
      queryText = queryText.toUpperCase();
    }

    for (String field :
        new String[] {
          PkgParserFlavor.INS.getPermName(this).toString(),
          ":" + getLocalizedProtectionLevel(),
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
      DaemonIface.INS.setAppOpMode(
          pkg.getUid(),
          isPerUid() ? null : pkg.getName(),
          AppOpsParser.INS.getAppOpsNames().indexOf(getName()),
          mode);
    }
  }

  public void toggleState(Package pkg) {
    if (!isAppOp()) {
      DaemonIface.INS.setPermState(
          !isGranted(), pkg.getName(), getName(), UserUtils.getUserId(pkg.getUid()));
    }
  }

  public String refString() {
    return isAppOp() ? refString(getAppOpMode()) : refString(isGranted());
  }

  public static String refString(boolean granted) {
    return granted ? Permission.GRANTED : Permission.REVOKED;
  }

  public static String refString(int appOpMode) {
    return AppOpsParser.INS.getAppOpsModes().get(appOpMode);
  }

  public static Boolean isReferenced(String savedRef, boolean granted) {
    return savedRef == null ? null : savedRef.equals(refString(granted));
  }

  public static Boolean isReferenced(String savedRef, int appOpMode) {
    return savedRef == null ? null : savedRef.equals(refString(appOpMode));
  }
}
