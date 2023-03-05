package com.mirfatif.permissionmanagerx.parser;

import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.text.TextUtils;
import android.text.format.DateUtils;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.MySettingsFlavor;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.LocaleUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class Package {

  private String mPackageLabel;
  private String mPackageName;
  private List<Permission> mPermissionsList, mSearchPermList;
  private boolean mIsFrameworkApp;
  private boolean mIsSystemApp;
  private boolean mIsEnabled;
  private int mUid;
  private Boolean mIsReferenced;
  private long mInstallDate;
  private long mUpdateDate;

  private int mTotalPermCount;
  private int mPermCount, mSearchPermCount;
  private int mTotalAppOpsCount;
  private int mAppOpsCount, mSearchAppOpsCount;

  private List<String> mPermFilter;

  void updatePackage(
      String label,
      String name,
      List<Permission> permissionList,
      boolean isFrameworkApp,
      boolean isSystemApp,
      boolean isEnabled,
      int uid,
      Boolean reference,
      long installDate,
      long updateDate) {
    mPackageLabel = label;
    mPackageName = name;
    mPermissionsList = permissionList;
    mIsFrameworkApp = isFrameworkApp;
    mIsSystemApp = isSystemApp;
    mIsEnabled = isEnabled;
    mUid = uid;
    mIsReferenced = reference;
    mInstallDate = installDate;
    mUpdateDate = updateDate;
  }

  private static long BUILD_DATE;

  static {
    try {
      PackageInfo pkgInfo = ApiUtils.getPkgInfo("android", 0);
      BUILD_DATE = Math.max(pkgInfo.firstInstallTime, pkgInfo.lastUpdateTime);
    } catch (NameNotFoundException ignored) {
      BUILD_DATE = System.currentTimeMillis() - TimeUnit.DAYS.toMillis(365);
    }
  }

  public String getLabel() {
    return mPackageLabel;
  }

  public String getName() {
    return mPackageName;
  }

  public String getFormattedName() {
    return getName() + " (" + getUid() + ")";
  }

  public List<Permission> getPermList() {
    if (MySettings.INS.isDeepSearching()) {
      return mSearchPermList == null ? new ArrayList<>() : mSearchPermList;
    }
    return getFullPermsList();
  }

  public List<Permission> getFullPermsList() {
    return mPermissionsList;
  }

  public void setSearchPermList(List<Permission> permList) {
    mSearchPermList = permList;
  }

  public boolean isFrameworkApp() {
    return mIsFrameworkApp;
  }

  public boolean isSystemApp() {
    return mIsSystemApp;
  }

  public boolean isEnabled() {
    return mIsEnabled;
  }

  public boolean isCriticalApp() {
    return ExcFiltersData.INS.isCriticalApp(mPackageName);
  }

  public boolean isChangeable() {
    return !isCriticalApp() && (!mIsFrameworkApp || MySettingsFlavor.INS.allowCriticalChanges());
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

  public void setSearchPermCount(int count) {
    mSearchPermCount = count;
  }

  private String mLastPermCount = "";

  public String getPermCount() {
    if (MySettings.INS.isDeepSearching()) {
      mLastPermCount =
          LocaleUtils.toLocalizedNum(mSearchPermCount)
              + "/"
              + LocaleUtils.toLocalizedNum(getTotalPermCount());
    } else {
      mLastPermCount =
          LocaleUtils.toLocalizedNum(mPermCount)
              + "/"
              + LocaleUtils.toLocalizedNum(getTotalPermCount());
    }
    if (AppOpsParser.INS.hasAppOps()) {
      mLastPermCount += " | " + getAppOpsCount();
    }
    return mLastPermCount;
  }

  public int getTotalPermAppOpCount() {
    return getTotalAppOpsCount() + getTotalPermCount();
  }

  public int getGrantedPermAppOpCount() {
    int granted = 0;
    for (Permission perm : getPermList()) {
      if (perm.isGranted()) {
        granted++;
      }
    }
    return granted;
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

  public void setSearchAppOpsCount(int count) {
    mSearchAppOpsCount = count;
  }

  private String getAppOpsCount() {
    if (MySettings.INS.isDeepSearching()) {
      return LocaleUtils.toLocalizedNum(mSearchAppOpsCount)
          + "/"
          + LocaleUtils.toLocalizedNum(getTotalAppOpsCount());
    } else {
      return LocaleUtils.toLocalizedNum(mAppOpsCount)
          + "/"
          + LocaleUtils.toLocalizedNum(getTotalAppOpsCount());
    }
  }

  public int getUid() {
    return mUid;
  }

  public Boolean isReferenced() {
    return mIsReferenced;
  }

  private boolean mLastShowingRef = true;

  public boolean shouldShowRefs() {
    mLastShowingRef = !MySettings.INS.isDeepSearching();
    return mLastShowingRef;
  }

  private String mLastDate;

  public String getDate() {
    Boolean isPkgInstalledDate = MySettingsFlavor.INS.isPkgInstallDate();
    if (isPkgInstalledDate == null) {
      mLastDate = null;
    } else {
      long date = isPkgInstalledDate ? mInstallDate : mUpdateDate;
      mLastDate =
          date > BUILD_DATE
              ? DateUtils.getRelativeTimeSpanString(
                      date, System.currentTimeMillis(), DateUtils.SECOND_IN_MILLIS)
                  .toString()
              : null;
    }
    return mLastDate;
  }

  public long getInstallTime() {
    return mInstallDate;
  }

  private boolean mPkgRemoved = false;

  public boolean isRemoved() {
    return mPkgRemoved;
  }

  public void setPkgRemoved(boolean isRemoved) {
    mPkgRemoved = isRemoved;
  }

  public List<String> getPermFilter() {
    return mPermFilter;
  }

  public void setPermFilter(List<String> permFilter) {
    mPermFilter = permFilter;
  }

  public boolean contains(String queryText) {
    if (!MySettings.INS.isSpecialSearch()) {
      return _contains(queryText);
    }

    boolean isEmpty = true;
    for (String str : queryText.split("\\|")) {
      if (TextUtils.isEmpty(str)) {
        continue;
      }
      isEmpty = false;
      if (contains_(str)) {
        return true;
      }
    }
    return isEmpty;
  }

  private boolean contains_(String queryText) {
    for (String str : queryText.split("&")) {
      if (TextUtils.isEmpty(str)) {
        continue;
      }
      if (!_contains(str)) {
        return false;
      }
    }
    return true;
  }

  private boolean _contains(String queryText) {
    boolean contains = true;
    if (MySettings.INS.isSpecialSearch() && queryText.startsWith("!")) {
      queryText = queryText.replaceAll("^!", "");
      contains = false;
    }

    Boolean handled = MySettingsFlavor.INS.handleSearchQuery(queryText, this, null);
    if (Boolean.TRUE.equals(handled)) {
      return contains;
    } else if (Boolean.FALSE.equals(handled)) {
      return !contains;
    }

    boolean isCaseSensitive = MySettings.INS.isCaseSensitiveSearch();
    if (!isCaseSensitive) {
      queryText = queryText.toUpperCase();
    }

    for (String field : searchableFields()) {
      if (!isCaseSensitive) {
        field = field.toUpperCase();
      }
      if (field.contains(queryText)) {
        return contains;
      }
    }
    return !contains;
  }

  private String[] searchableFields() {
    return new String[] {
      mPackageLabel,
      mPackageName,
      String.valueOf(mUid),
      (isCriticalApp()
          ? SearchConstants.INS.SEARCH_CRITICAL
          : (mIsFrameworkApp
              ? SearchConstants.INS.SEARCH_FRAMEWORK
              : (mIsSystemApp
                  ? SearchConstants.INS.SEARCH_SYSTEM
                  : SearchConstants.INS.SEARCH_USER))),
      (mIsEnabled ? "" : SearchConstants.INS.SEARCH_DISABLED),
      (mIsReferenced == null
          ? SearchConstants.INS.SEARCH_ORANGE
          : (mIsReferenced ? SearchConstants.INS.SEARCH_GREEN : SearchConstants.INS.SEARCH_RED))
    };
  }

  public boolean areContentsTheSame(Package pkg) {
    boolean lastShowingRef = mLastShowingRef;
    if (lastShowingRef != pkg.shouldShowRefs()) {
      return false;
    }

    if (!Objects.equals(isReferenced(), pkg.isReferenced())) {
      return false;
    }

    if (!mLastPermCount.equals(pkg.getPermCount())) {
      return false;
    }

    if (!Objects.equals(mLastDate, pkg.getDate())) {
      return false;
    }

    return isEnabled() == pkg.isEnabled();
  }
}
