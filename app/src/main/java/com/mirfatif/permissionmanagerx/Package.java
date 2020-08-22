package com.mirfatif.permissionmanagerx;

import android.graphics.drawable.Drawable;
import java.util.List;

public class Package {

  private String mPackageLabel;
  private String mPackageName;
  private List<Permission> mPermissionsList;
  private boolean mIsFrameworkApp;
  private boolean mIsSystemApp;
  private boolean mIsEnabled;
  private Drawable mIcon;
  private int mUid;
  private Boolean mIsReferenced;

  private int mTotalPermCount;
  private int mPermCount;
  private int mTotalAppOpsCount;
  private int mAppOpsCount;

  void updatePackage(
      String label,
      String name,
      List<Permission> permissionList,
      boolean isFrameworkApp,
      boolean isSystemApp,
      boolean isEnabled,
      Drawable icon,
      int uid,
      Boolean reference) {
    mPackageLabel = label;
    mPackageName = name;
    mPermissionsList = permissionList;
    mIsFrameworkApp = isFrameworkApp;
    mIsSystemApp = isSystemApp;
    mIsEnabled = isEnabled;
    mIcon = icon;
    mUid = uid;
    mIsReferenced = reference;
  }

  String getLabel() {
    return mPackageLabel;
  }

  String getName() {
    return mPackageName;
  }

  List<Permission> getPermissionsList() {
    return mPermissionsList;
  }

  boolean isFrameworkApp() {
    return mIsFrameworkApp;
  }

  boolean isSystemApp() {
    return mIsSystemApp;
  }

  boolean isEnabled() {
    return mIsEnabled;
  }

  boolean isCriticalApp() {
    return MySettings.getInstance().isCriticalApp(mPackageName);
  }

  boolean isChangeable() {
    return !isCriticalApp() && !mIsFrameworkApp;
  }

  void setTotalPermCount(int count) {
    mTotalPermCount = count;
  }

  int getTotalPermCount() {
    return mTotalPermCount;
  }

  void setPermCount(int count) {
    mPermCount = count;
  }

  int getPermCount() {
    return mPermCount;
  }

  void setTotalAppOpsCount(int count) {
    mTotalAppOpsCount = count;
  }

  int getTotalAppOpsCount() {
    return mTotalAppOpsCount;
  }

  void setAppOpsCount(int count) {
    mAppOpsCount = count;
  }

  int getAppOpsCount() {
    return mAppOpsCount;
  }

  Drawable getIcon() {
    return mIcon;
  }

  int getUid() {
    return mUid;
  }

  Boolean isReferenced() {
    return mIsReferenced;
  }

  boolean contains(String queryText) {
    boolean isCaseSensitive = MySettings.getInstance().isCaseSensitiveSearch();
    if (!isCaseSensitive) queryText = queryText.toUpperCase();

    for (String field :
        new String[] {
          mPackageLabel,
          mPackageName,
          String.valueOf(mUid),
          (isCriticalApp()
              ? ":Critical"
              : (mIsFrameworkApp ? ":Framework" : (mIsSystemApp ? ":System" : ":User"))),
          (mIsEnabled ? "" : ":Disabled"),
          (mIsReferenced == null ? ":ORANGE" : (mIsReferenced ? ":GREEN" : ":RED"))
        }) {
      if (!isCaseSensitive) field = field.toUpperCase();
      if (field.contains(queryText)) return true;
    }
    return false;
  }

  // for ListAdapter/DiffUtil
  // consider which fields can change
  boolean areContentsTheSame(Package pkg) {

    if (pkg.isReferenced() != null && this.isReferenced() != null) {
      if (pkg.isReferenced().compareTo(this.isReferenced()) != 0) return false;
    } else if (pkg.isReferenced() == null && this.isReferenced() != null) {
      return false;
    } else if (pkg.isReferenced() != null && this.isReferenced() == null) {
      return false;
    }

    if (!pkg.getName().equals(this.getName())) return false;
    if (!pkg.getPermissionsList().equals(this.getPermissionsList())) return false;
    return Boolean.compare(pkg.isEnabled(), this.isEnabled()) == 0;
  }
}
