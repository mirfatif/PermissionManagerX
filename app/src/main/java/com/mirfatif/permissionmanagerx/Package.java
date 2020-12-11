package com.mirfatif.permissionmanagerx;

import android.graphics.drawable.Drawable;
import android.text.TextUtils;
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

  public void updatePackage(
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

  public String getLabel() {
    return mPackageLabel;
  }

  public String getName() {
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

  public void setTotalPermCount(int count) {
    mTotalPermCount = count;
  }

  public int getTotalPermCount() {
    return mTotalPermCount;
  }

  public void setPermCount(int count) {
    mPermCount = count;
  }

  public int getPermCount() {
    return mPermCount;
  }

  public void setTotalAppOpsCount(int count) {
    mTotalAppOpsCount = count;
  }

  public int getTotalAppOpsCount() {
    return mTotalAppOpsCount;
  }

  public void setAppOpsCount(int count) {
    mAppOpsCount = count;
  }

  public int getAppOpsCount() {
    return mAppOpsCount;
  }

  Drawable getIcon() {
    return mIcon;
  }

  int getUid() {
    return mUid;
  }

  public Boolean isReferenced() {
    return mIsReferenced;
  }

  public static final String SEARCH_CRITICAL = ":Critical";
  public static final String SEARCH_FRAMEWORK = ":Framework";
  public static final String SEARCH_SYSTEM = ":System";
  public static final String SEARCH_USER = ":User";
  public static final String SEARCH_DISABLED = ":Disabled";
  public static final String SEARCH_GREEN = ":GREEN";
  public static final String SEARCH_ORANGE = ":ORANGE";
  public static final String SEARCH_RED = ":RED";

  public boolean contains(String queryText) {
    boolean isEmpty = true;
    for (String str : queryText.split("\\|")) {
      if (TextUtils.isEmpty(str)) continue;
      isEmpty = false;
      if (contains_(str)) return true;
    }
    return isEmpty;
  }

  private boolean contains_(String queryText) {
    for (String str : queryText.split("&")) {
      if (TextUtils.isEmpty(str)) continue;
      if (!_contains(str)) return false;
    }
    return true;
  }

  private boolean _contains(String queryText) {
    boolean isCaseSensitive = MySettings.getInstance().isCaseSensitiveSearch();
    if (!isCaseSensitive) queryText = queryText.toUpperCase();

    for (String field :
        new String[] {
          mPackageLabel,
          mPackageName,
          String.valueOf(mUid),
          (isCriticalApp()
              ? SEARCH_CRITICAL
              : (mIsFrameworkApp
                  ? SEARCH_FRAMEWORK
                  : (mIsSystemApp ? SEARCH_SYSTEM : SEARCH_USER))),
          (mIsEnabled ? "" : SEARCH_DISABLED),
          (mIsReferenced == null ? SEARCH_ORANGE : (mIsReferenced ? SEARCH_GREEN : SEARCH_RED))
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
