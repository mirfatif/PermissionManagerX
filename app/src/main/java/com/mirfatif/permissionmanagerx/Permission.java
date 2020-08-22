package com.mirfatif.permissionmanagerx;

import android.text.format.DateUtils;

class Permission {

  static final String PROTECTION_DANGEROUS = "Dangerous";
  static final String PROTECTION_NORMAL = "Normal";
  static final String PROTECTION_SIGNATURE = "Signature";
  static final String GRANTED = "Granted";
  static final String REVOKED = "Revoked";

  private final int mOrder;
  private final int mIconResId;
  private final boolean mIsAppOps;
  private final boolean mIsPerUid;
  private final boolean mIsAppOpsSet;
  private int mAppOpsMode;
  private final long mAppOpsAccessTime;
  private String mAppOpsAccessTimeFormatted;
  private final String mDependsOn;
  private boolean mIsExtraAppOp;
  private final String mPackageName;
  private final String mPermissionName;
  private final boolean mIsGranted;
  private final String mProtectionLevel;
  private final boolean mIsPrivileged;
  private final boolean mIsDevelopment;
  private final boolean mIsManifestPermAppOps;
  private final boolean mIsSystemFixed;
  private final boolean mProviderMissing;
  private final Boolean mIsReferenced;
  private final String mReference;
  private final boolean mIsSystemApp;
  private final CharSequence mPermDesc;

  Permission(
      int order,
      int iconResId,
      boolean isAppOps,
      boolean isPerUid,
      boolean isAppOpsSet,
      int appOpsMode,
      long appOpsAccessTime,
      String dependsOn,
      boolean isExtraAppOp,
      String packageName,
      String name,
      boolean isGranted,
      String protectionLevel,
      boolean isPrivileged,
      boolean isDevelopment,
      boolean isManifestPermAppOps,
      boolean isSystemFixed,
      boolean providerMissing,
      Boolean isReferenced,
      String reference,
      boolean isSystemApp,
      CharSequence permDesc) {
    mOrder = order;
    mIconResId = iconResId;
    mIsAppOps = isAppOps;
    mIsPerUid = isPerUid;
    mIsAppOpsSet = isAppOpsSet;
    mAppOpsMode = appOpsMode;
    mAppOpsAccessTime = appOpsAccessTime;
    mDependsOn = dependsOn;
    mIsExtraAppOp = isExtraAppOp;
    mPackageName = packageName;
    mPermissionName = name;
    mIsGranted = isGranted;
    mProtectionLevel = protectionLevel;
    mIsPrivileged = isPrivileged;
    mIsDevelopment = isDevelopment;
    mIsManifestPermAppOps = isManifestPermAppOps;
    mIsSystemFixed = isSystemFixed;
    mProviderMissing = providerMissing;
    mIsReferenced = isReferenced;
    mReference = reference;
    mIsSystemApp = isSystemApp;
    mPermDesc = permDesc;
  }

  int getOrder() {
    return mOrder;
  }

  int getIconResId() {
    return mIconResId;
  }

  boolean isAppOps() {
    return mIsAppOps;
  }

  boolean isPerUid() {
    return mIsPerUid;
  }

  boolean isAppOpsSet() {
    return mIsAppOpsSet;
  }

  void setAppOpsMode(int mode) {
    mAppOpsMode = mode;
  }

  int getAppOpsMode() {
    return mAppOpsMode;
  }

  String getAppOpsAccessTime() {
    // do not show time older than a year, including zero epoch time and -1
    if (System.currentTimeMillis() - mAppOpsAccessTime > (365 * 24 * 60 * 60 * 1000L)) {
      return "null";
    }
    if (mAppOpsAccessTimeFormatted == null) {
      mAppOpsAccessTimeFormatted =
          DateUtils.getRelativeTimeSpanString(
                  mAppOpsAccessTime, System.currentTimeMillis(), DateUtils.SECOND_IN_MILLIS)
              .toString();
    }
    return mAppOpsAccessTimeFormatted;
  }

  String dependsOn() {
    return mDependsOn;
  }

  void setExtraAppOp() {
    mIsExtraAppOp = true;
  }

  boolean isExtraAppOp() {
    return mIsExtraAppOp;
  }

  String getName() {
    return mPermissionName;
  }

  boolean isGranted() {
    return mIsGranted;
  }

  String getProtectionLevel() {
    return mProtectionLevel;
  }

  boolean isPrivileged() {
    return mIsPrivileged;
  }

  boolean isProviderMissing() {
    return mProviderMissing;
  }

  Boolean isReferenced() {
    return mIsReferenced;
  }

  String getReference() {
    return mReference;
  }

  boolean isChangeable() {
    if (MySettings.getInstance().isCriticalApp(mPackageName)) return false;
    if (mIsAppOps) {
      return mDependsOn == null;
    } else {
      // BasePermission.java#enforceDeclaredUsedAndRuntimeOrDevelopment()
      return (mProtectionLevel.equals(PROTECTION_DANGEROUS) || mIsDevelopment)
          && (!mIsSystemApp || !mIsPrivileged)
          && !mIsSystemFixed;
    }
  }

  CharSequence getDescription() {
    return mPermDesc;
  }

  String createPermNameString() {
    String permName = mPermissionName;
    if (mIsAppOps && mDependsOn != null) {
      permName += " (" + mDependsOn + ")";
    }
    return permName;
  }

  String createProtectLevelString() {
    String protectionLevel = mProtectionLevel;
    if (mIsDevelopment) protectionLevel += ", Development"; // implies "Signature"
    if (mIsManifestPermAppOps) protectionLevel += ", AppOps"; // implies "Signature"
    if (mIsSystemFixed) protectionLevel += ", Fixed";

    if (mIsAppOps) {
      if (mIsPerUid) protectionLevel += ", UID mode";
      if (mIsExtraAppOp) protectionLevel += ", Extra";
    } else {
      if (mIsPrivileged) protectionLevel += ", Privileged";
    }
    return protectionLevel;
  }

  boolean contains(String queryText) {
    boolean isCaseSensitive = MySettings.getInstance().isCaseSensitiveSearch();
    if (!isCaseSensitive) queryText = queryText.toUpperCase();

    for (String field :
        new String[] {
          mPermissionName,
          ":" + mProtectionLevel,
          ((mIsAppOps || mIsManifestPermAppOps) ? ":AppOps" : ""),
          ((mIsAppOps && mIsPerUid) ? ":UID" : ""),
          (mIsPrivileged ? ":Privileged" : ""),
          (mIsDevelopment ? ":Development" : ""),
          (mIsSystemFixed ? ":Fixed" : ""),
          (mIsReferenced == null ? ":ORANGE" : (mIsReferenced ? ":GREEN" : ":RED")),
          (!getAppOpsAccessTime().equals("null") ? ":TIME" : ""),
          (mIsExtraAppOp ? ":Extra" : "")
        }) {
      if (!isCaseSensitive) field = field.toUpperCase();
      if (field.contains(queryText)) return true;
    }
    return false;
  }

  // required for ListAdapter/DiffUtil
  // consider which fields can change
  boolean areContentsTheSame(Permission permission) {
    if (!permission.getName().equals(this.getName())) return false;

    if (permission.isReferenced() != null && this.isReferenced() != null) {
      if (permission.isReferenced().compareTo(this.isReferenced()) != 0) return false;
    } else if (permission.isReferenced() == null && this.isReferenced() != null) {
      return false;
    } else if (permission.isReferenced() != null && this.isReferenced() == null) {
      return false;
    }

    if (isAppOps()) {
      if (permission.getAppOpsMode() != this.getAppOpsMode()) return false;
      if (Boolean.compare(permission.isAppOpsSet(), this.isAppOpsSet()) != 0) return false;
      if (!permission.getAppOpsAccessTime().equals(this.getAppOpsAccessTime())) return false;
      return Boolean.compare(permission.isExtraAppOp(), this.isExtraAppOp()) == 0;
    } else {
      return Boolean.compare(permission.isGranted(), this.isGranted()) == 0;
    }
  }
}
