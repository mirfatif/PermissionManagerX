package com.mirfatif.permissionmanagerx.parser;

import android.text.TextUtils;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.util.LocaleUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Package {

  private String mPackageLabel;
  private String mPackageName;
  private List<Permission> mPermissionsList, mSearchPermList;
  private boolean mIsFrameworkApp;
  private boolean mIsSystemApp;
  private boolean mIsEnabled;
  private int mUid;
  private Boolean mIsReferenced;

  private int mTotalPermCount;
  private int mPermCount, mSearchPermCount;
  private int mTotalAppOpsCount;
  private int mAppOpsCount, mSearchAppOpsCount;

  void updatePackage(
      String label,
      String name,
      List<Permission> permissionList,
      boolean isFrameworkApp,
      boolean isSystemApp,
      boolean isEnabled,
      int uid,
      Boolean reference) {
    mPackageLabel = label;
    mPackageName = name;
    mPermissionsList = permissionList;
    mIsFrameworkApp = isFrameworkApp;
    mIsSystemApp = isSystemApp;
    mIsEnabled = isEnabled;
    mUid = uid;
    mIsReferenced = reference;
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

  public String getDate() {
    return null;
  }

  private boolean mPkgRemoved = false;

  public boolean isRemoved() {
    return mPkgRemoved;
  }

  public void setPkgRemoved(boolean isRemoved) {
    mPkgRemoved = isRemoved;
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

    return isEnabled() == pkg.isEnabled();
  }
}
