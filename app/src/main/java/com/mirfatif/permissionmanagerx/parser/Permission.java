package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.permissionmanagerx.parser.SearchConstants.CONSTANTS;
import static com.mirfatif.permissionmanagerx.util.Utils.getString;

import android.text.TextUtils;
import android.text.format.DateUtils;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.MySettingsFlavor;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import java.util.concurrent.TimeUnit;

public class Permission {

  private static final String TAG = "Permission";

  private final MySettings mMySettings = MySettings.getInstance();
  private final MySettingsFlavor mMySettingsFlavor = MySettingsFlavor.getInstance();

  public static final String PROTECTION_UNKNOWN = "PROTECTION_UNKNOWN";
  public static final String PROTECTION_DANGEROUS = "PROTECTION_DANGEROUS";
  public static final String PROTECTION_NORMAL = "PROTECTION_NORMAL";
  public static final String PROTECTION_SIGNATURE = "PROTECTION_SIGNATURE";

  public static final String GRANTED = "GRANTED";
  public static final String REVOKED = "REVOKED";

  private static final String APP_OPS = "APP_OPS";

  // Common
  private final int mOrder;
  private final Integer mIconResId;
  private final String mPackageName;
  private final String mPermissionName;
  private final boolean mIsGranted;
  private final Boolean mIsReferenced;
  private final String mReference;
  private final boolean mIsSystemApp, mIsFrameworkApp;

  // Manifest permissions
  private final String mProtectionLevel;
  private final boolean mIsPrivileged;
  private final boolean mIsDevelopment;
  private final boolean mIsManifestPermAppOp;
  private final boolean mIsSystemFixed, mIsPolicyFixed;
  private final boolean mProviderMissing;
  private final CharSequence mPermDesc;

  // AppOps
  private final boolean mIsAppOps;
  private final boolean mIsPerUid;
  private final boolean mIsAppOpsSet;
  private int mAppOpsMode;
  private final long mAppOpsAccessTime;
  private String mAppOpsAccessTimeFormatted;
  private final String mDependsOn;
  private boolean mIsExtraAppOp;

  Permission(
      int order,
      Integer iconResId,
      String packageName,
      String name,
      boolean isGranted,
      Boolean isReferenced,
      String reference,
      boolean isSystemApp,
      boolean isFrameworkApp,
      String protectionLevel,
      boolean isPrivileged,
      boolean isDevelopment,
      boolean isManifestPermAppOp,
      boolean isSystemFixed,
      boolean isPolicyFixed,
      boolean providerMissing,
      CharSequence permDesc) {
    mOrder = order;
    mIconResId = iconResId;
    mPackageName = packageName;
    mPermissionName = name;
    mIsGranted = isGranted;
    mIsReferenced = isReferenced;
    mReference = reference;
    mIsSystemApp = isSystemApp;
    mIsFrameworkApp = isFrameworkApp;
    mProtectionLevel = protectionLevel;
    mIsPrivileged = isPrivileged;
    mIsDevelopment = isDevelopment;
    mIsManifestPermAppOp = isManifestPermAppOp;
    mIsSystemFixed = isSystemFixed;
    mIsPolicyFixed = isPolicyFixed;
    mProviderMissing = providerMissing;
    mPermDesc = permDesc;

    mIsAppOps = false;
    mIsPerUid = false;
    mIsAppOpsSet = false;
    mAppOpsMode = -1;
    mAppOpsAccessTime = -1;
    mDependsOn = null;
    mIsExtraAppOp = false;
  }

  Permission(
      int order,
      Integer iconResId,
      String packageName,
      String name,
      boolean isGranted,
      Boolean isReferenced,
      String reference,
      boolean isSystemApp,
      boolean isFrameworkApp,
      boolean isPerUid,
      boolean isAppOpsSet,
      int appOpsMode,
      long appOpsAccessTime,
      String dependsOn,
      boolean isExtraAppOp) {
    mOrder = order;
    mIconResId = iconResId;
    mPackageName = packageName;
    mPermissionName = name;
    mIsGranted = isGranted;
    mIsReferenced = isReferenced;
    mReference = reference;
    mIsSystemApp = isSystemApp;
    mIsFrameworkApp = isFrameworkApp;
    mIsPerUid = isPerUid;
    mIsAppOpsSet = isAppOpsSet;
    mAppOpsMode = appOpsMode;
    mAppOpsAccessTime = appOpsAccessTime;
    mDependsOn = dependsOn;
    mIsExtraAppOp = isExtraAppOp;

    mIsAppOps = true;
    mProtectionLevel = APP_OPS;
    mIsPrivileged = false;
    mIsDevelopment = false;
    mIsManifestPermAppOp = false;
    mIsSystemFixed = false;
    mIsPolicyFixed = false;
    mProviderMissing = false;
    mPermDesc = null;
  }

  public int getOrder() {
    return mOrder;
  }

  public Integer getIconResId() {
    return mIconResId;
  }

  public boolean isAppOps() {
    return mIsAppOps;
  }

  public boolean isPerUid() {
    return mIsPerUid;
  }

  public boolean isAppOpsSet() {
    return mIsAppOpsSet;
  }

  public void setAppOpsMode(int mode) {
    mAppOpsMode = mode;
  }

  public int getAppOpsMode() {
    return mAppOpsMode;
  }

  public String getAppOpsAccessTime() {
    // Do not show time older than a year, including zero epoch time and -1
    if (System.currentTimeMillis() - mAppOpsAccessTime > TimeUnit.DAYS.toMillis(365)) {
      return null;
    }
    mAppOpsAccessTimeFormatted =
        DateUtils.getRelativeTimeSpanString(
                mAppOpsAccessTime, System.currentTimeMillis(), DateUtils.SECOND_IN_MILLIS)
            .toString();
    return mAppOpsAccessTimeFormatted;
  }

  public String dependsOn() {
    return mDependsOn;
  }

  public void setExtraAppOp() {
    mIsExtraAppOp = true;
  }

  public boolean isExtraAppOp() {
    return mIsExtraAppOp;
  }

  public String getName() {
    return mPermissionName;
  }

  public boolean isGranted() {
    return mIsGranted;
  }

  public String getProtectionLevel() {
    return mProtectionLevel;
  }

  public boolean isPrivileged() {
    return mIsPrivileged;
  }

  public boolean isProviderMissing() {
    return mProviderMissing;
  }

  public boolean isSystemFixed() {
    return mIsSystemFixed;
  }

  public Boolean isReferenced() {
    return mIsReferenced;
  }

  public String getReference() {
    return mReference;
  }

  public boolean isCritical() {
    return (mIsSystemApp && mIsPrivileged) || mIsSystemFixed;
  }

  private Boolean mIsChangeable = null;

  public boolean isChangeable() {
    if (mIsChangeable == null) {
      mIsChangeable = _isChangeable();
    }
    return mIsChangeable;
  }

  private boolean _isChangeable() {
    boolean allowCriticChanges = MySettingsFlavor.getInstance().allowCriticalChanges();
    if ((mIsFrameworkApp && !allowCriticChanges) || mMySettings.isCriticalApp(mPackageName)) {
      return false;
    }
    if (mIsAppOps) {
      return mDependsOn == null;
    } else {
      // BasePermission.java#enforceDeclaredUsedAndRuntimeOrDevelopment()
      return (mProtectionLevel.equals(PROTECTION_DANGEROUS) || mIsDevelopment)
          && (!mIsSystemApp || !mIsPrivileged || allowCriticChanges)
          && !mIsPolicyFixed
          && (!mIsSystemFixed
              || (allowCriticChanges && PrivDaemonHandler.getInstance().isSystemUid()));
    }
  }

  public CharSequence getDescription() {
    return mPermDesc;
  }

  public String getPermNameString() {
    String permName = mPermissionName;
    if (mIsAppOps && mDependsOn != null) {
      permName += " (" + mDependsOn + ")";
    }
    return permName;
  }

  private String getLocalizedProtectionLevel() {
    switch (mProtectionLevel) {
      case PROTECTION_UNKNOWN:
      default:
        return getString(R.string.prot_lvl_unknown);
      case PROTECTION_NORMAL:
        return getString(R.string.prot_lvl_normal);
      case PROTECTION_DANGEROUS:
        return getString(R.string.prot_lvl_dangerous);
      case PROTECTION_SIGNATURE:
        return getString(R.string.prot_lvl_signature);
      case APP_OPS:
        return getString(R.string.prot_lvl_app_ops);
    }
  }

  public String getProtLevelString() {
    String protectionLevel = getLocalizedProtectionLevel();

    if (mIsAppOps) {
      if (mIsPerUid) {
        protectionLevel = getString(R.string.prot_lvl_uid_mode, protectionLevel);
      }
      if (mIsExtraAppOp) {
        protectionLevel = getString(R.string.prot_lvl_extra, protectionLevel);
      }
    } else {
      if (mIsDevelopment) { // Implies "Signature"
        protectionLevel = getString(R.string.prot_lvl_development2, protectionLevel);
      }
      if (mIsManifestPermAppOp) { // Implies "Signature"
        protectionLevel = getString(R.string.prot_lvl_app_ops2, protectionLevel);
      }
      if (mIsPrivileged) { // Implies "Signature"
        protectionLevel = getString(R.string.prot_lvl_privileged2, protectionLevel);
      }
      if (mIsSystemFixed || mIsPolicyFixed) {
        protectionLevel = getString(R.string.prot_lvl_fixed2, protectionLevel);
      }
    }
    return protectionLevel;
  }

  public boolean contains(Package pkg, String queryText) {
    if (!mMySettings.isSpecialSearch()) {
      return _contains(pkg, queryText);
    }

    boolean isEmpty = true;
    for (String str : queryText.split("\\|")) {
      if (TextUtils.isEmpty(str)) {
        continue;
      }
      isEmpty = false;
      if (contains_(pkg, str)) {
        return true;
      }
    }
    return isEmpty;
  }

  private boolean contains_(Package pkg, String queryText) {
    for (String str : queryText.split("&")) {
      if (TextUtils.isEmpty(str)) {
        continue;
      }
      if (!_contains(pkg, str)) {
        return false;
      }
    }
    return true;
  }

  private boolean _contains(Package pkg, String queryText) {
    boolean contains = true;
    if (mMySettings.isSpecialSearch() && queryText.startsWith("!")) {
      queryText = queryText.replaceAll("^!", "");
      contains = false;
    }

    Boolean handled = mMySettingsFlavor.handleSearchQuery(queryText, pkg, this);
    if (handled != null) {
      if (handled) {
        return contains;
      } else {
        return !contains;
      }
    }

    queryText = queryText.toUpperCase();

    for (String field :
        new String[] {
          mPermissionName,
          ":" + getLocalizedProtectionLevel(),
          ((mIsAppOps || mIsManifestPermAppOp) ? CONSTANTS.SEARCH_APP_OPS : ""),
          ((mIsAppOps && mIsPerUid) ? CONSTANTS.SEARCH_UID : ""),
          (mIsPrivileged ? CONSTANTS.SEARCH_PRIVILEGED : ""),
          (mIsDevelopment ? CONSTANTS.SEARCH_DEV : ""),
          (mIsSystemFixed ? CONSTANTS.SEARCH_FIXED : ""),
          (mIsReferenced == null
              ? CONSTANTS.SEARCH_ORANGE
              : (mIsReferenced ? CONSTANTS.SEARCH_GREEN : CONSTANTS.SEARCH_RED)),
          getAppOpsAccessTime() != null ? CONSTANTS.SEARCH_TIME : "",
          (mIsExtraAppOp ? CONSTANTS.SEARCH_EXTRA : "")
        }) {
      if (field.toUpperCase().contains(queryText)) {
        return contains;
      }
    }
    return !contains;
  }

  // Required for ListAdapter/DiffUtil. Consider which fields can change in a Permission.
  public boolean areContentsTheSame(Permission newPerm) {
    if (isReferenced() != null) {
      if (!isReferenced().equals(newPerm.isReferenced())) {
        return false;
      }
    } else if (newPerm.isReferenced() != null) {
      return false;
    }

    if (!isAppOps()) {
      return isGranted() == newPerm.isGranted();
    }

    if (getAppOpsMode() != newPerm.getAppOpsMode()) {
      return false;
    }

    if (isAppOpsSet() != newPerm.isAppOpsSet()) {
      return false;
    }

    // Compare the new Permission Object's calculated value with previously one's saved value,
    // even if both Objects are the same.
    // If calculated on both, the values are always the same, so UI changes cannot be compared.
    if (mAppOpsAccessTimeFormatted != null) {
      if (!mAppOpsAccessTimeFormatted.equals(newPerm.getAppOpsAccessTime())) {
        return false;
      }
    } else if (newPerm.getAppOpsAccessTime() != null) {
      return false;
    }

    return isExtraAppOp() == newPerm.isExtraAppOp();
  }

  public static void setAppOpMode(Package pkg, Permission perm, int mode) {
    String pkgName;
    if (perm.isPerUid()) {
      pkgName = "null";
    } else {
      pkgName = pkg.getName();
    }
    String command =
        Commands.SET_APP_OPS_MODE
            + " "
            + perm.getName()
            + " "
            + pkg.getUid()
            + " "
            + pkgName
            + " "
            + mode;
    if (MySettings.getInstance().isDebug()) {
      Util.debugLog(TAG, "setAppOpsMode: sending command: " + command);
    }
    PrivDaemonHandler.getInstance().sendRequest(command);
  }

  public static void setPermission(Package pkg, Permission perm) {
    String command = pkg.getName() + " " + perm.getName() + " " + Utils.getUserId(pkg.getUid());
    if (perm.isGranted()) {
      command = Commands.REVOKE_PERMISSION + " " + command;
    } else {
      command = Commands.GRANT_PERMISSION + " " + command;
    }

    if (MySettings.getInstance().isDebug()) {
      Util.debugLog(TAG, "setPermission: sending command: " + command);
    }
    PrivDaemonHandler.getInstance().sendRequest(command);
  }
}
