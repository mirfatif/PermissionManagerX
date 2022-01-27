package com.mirfatif.permissionmanagerx.parser;

import android.annotation.SuppressLint;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.text.TextUtils;
import android.text.format.DateUtils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.MySettingsFlavor;
import java.io.File;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
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
      PackageManager pm = App.getContext().getPackageManager();
      PackageInfo pkgInfo = pm.getPackageInfo("android", 0);
      ApplicationInfo appInfo = pkgInfo.applicationInfo;
      BUILD_DATE = Math.max(pkgInfo.firstInstallTime, new File(appInfo.sourceDir).lastModified());
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

  private String mLastFormattedName = "";

  public String getFormattedName() {
    mLastFormattedName = getName();
    if (!MySettings.INSTANCE.isQuickScanEnabled()) {
      mLastFormattedName += " (" + getUid() + ")";
    }
    return mLastFormattedName;
  }

  public List<Permission> getPermissionsList() {
    if (MySettings.INSTANCE.isDeepSearching()) {
      if (mSearchPermList == null) {
        return new ArrayList<>();
      }
      return mSearchPermList;
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
    return MySettings.INSTANCE.isCriticalApp(mPackageName);
  }

  public boolean isChangeable() {
    return !isCriticalApp()
        && (!mIsFrameworkApp || MySettingsFlavor.INSTANCE.allowCriticalChanges());
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
    if (MySettings.INSTANCE.isQuickScanEnabled()) {
      return String.valueOf(getUid());
    }
    NumberFormat nf = NumberFormat.getIntegerInstance();
    if (MySettings.INSTANCE.isDeepSearching()) {
      mLastPermCount = nf.format(mSearchPermCount) + "/" + nf.format(getTotalPermCount());
    } else {
      mLastPermCount = nf.format(mPermCount) + "/" + nf.format(getTotalPermCount());
    }
    if (!MySettings.INSTANCE.excludeAppOpsPerms()) {
      mLastPermCount += " | " + getAppOpsCount(nf);
    }
    return mLastPermCount;
  }

  @SuppressWarnings("UnusedDeclaration")
  public int getTotalPermAppOpCount() {
    if (MySettings.INSTANCE.isQuickScanEnabled()) {
      return 0;
    }
    return getTotalAppOpsCount() + getTotalPermCount();
  }

  @SuppressWarnings("UnusedDeclaration")
  public int getGrantedPermAppOpCount() {
    if (MySettings.INSTANCE.isQuickScanEnabled()) {
      return 0;
    }
    int granted = 0;
    for (Permission perm : getPermissionsList()) {
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

  private String getAppOpsCount(NumberFormat nf) {
    if (MySettings.INSTANCE.isDeepSearching()) {
      return nf.format(mSearchAppOpsCount) + "/" + nf.format(getTotalAppOpsCount());
    } else {
      return nf.format(mAppOpsCount) + "/" + nf.format(getTotalAppOpsCount());
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
    mLastShowingRef = MySettings.INSTANCE.shouldShowRefs();
    return mLastShowingRef;
  }

  private String mLastDate;

  public String getDate() {
    Boolean isPkgInstalledDate = MySettingsFlavor.INSTANCE.isPkgInstallDate();
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

  @SuppressWarnings("UnusedDeclaration")
  public long getInstallTime() {
    return mInstallDate;
  }

  private boolean mIsRemoved = false;

  public boolean isRemoved() {
    return mIsRemoved;
  }

  public void setIsRemoved(boolean isRemoved) {
    mIsRemoved = isRemoved;
  }

  @SuppressWarnings("UnusedDeclaration")
  public List<String> getPermFilter() {
    return mPermFilter;
  }

  @SuppressWarnings("UnusedDeclaration")
  public void setPermFilter(List<String> permFilter) {
    mPermFilter = permFilter;
  }

  public boolean contains(String queryText) {
    if (!MySettings.INSTANCE.isSpecialSearch()) {
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
    if (MySettings.INSTANCE.isSpecialSearch() && queryText.startsWith("!")) {
      queryText = queryText.replaceAll("^!", "");
      contains = false;
    }

    Boolean handled = MySettingsFlavor.INSTANCE.handleSearchQuery(queryText, this, null);
    if (Boolean.TRUE.equals(handled)) {
      return contains;
    } else if (Boolean.FALSE.equals(handled)) {
      return !contains;
    }

    boolean isCaseSensitive = MySettings.INSTANCE.isCaseSensitiveSearch();
    if (!isCaseSensitive) {
      queryText = queryText.toUpperCase();
    }

    for (String field :
        new String[] {
          mPackageLabel,
          mPackageName,
          String.valueOf(mUid),
          (isCriticalApp()
              ? SearchConstants.INSTANCE.SEARCH_CRITICAL
              : (mIsFrameworkApp
                  ? SearchConstants.INSTANCE.SEARCH_FRAMEWORK
                  : (mIsSystemApp
                      ? SearchConstants.INSTANCE.SEARCH_SYSTEM
                      : SearchConstants.INSTANCE.SEARCH_USER))),
          (mIsEnabled ? "" : SearchConstants.INSTANCE.SEARCH_DISABLED),
          (MySettings.INSTANCE.isQuickScanEnabled()
              ? ""
              : (mIsReferenced == null
                  ? SearchConstants.INSTANCE.SEARCH_ORANGE
                  : (mIsReferenced
                      ? SearchConstants.INSTANCE.SEARCH_GREEN
                      : SearchConstants.INSTANCE.SEARCH_RED)))
        }) {
      if (!isCaseSensitive) {
        field = field.toUpperCase();
      }
      if (field.contains(queryText)) {
        return contains;
      }
    }
    return !contains;
  }

  /*
    For ListAdapter / DiffUtil.
    Consider which fields can change when the Package Object changes and when it remains same.
    When the Package remains same e.g. while searching, in order to compare the UI-only changes,
    we need to retain the last returned values of changeable fields for later comparison.
    When ref state or enabled state changes, Activity gets Live Package changed update. So no
    need to retain their states.
    boolean is not immutable like String, so preserve the old value before overwriting the Object.
  */
  public boolean areContentsTheSame(Package pkg) {
    if (!MySettings.INSTANCE.isQuickScanEnabled()) {
      if (getNewBoolean(mLastShowingRef) != pkg.shouldShowRefs()) {
        return false;
      }

      if (isReferenced() != null) {
        if (!isReferenced().equals(pkg.isReferenced())) {
          return false;
        }
      } else if (pkg.isReferenced() != null) {
        return false;
      }

      if (!mLastPermCount.equals(pkg.getPermCount())) {
        return false;
      }
    }

    // This necessarily changes when we change QuickScan settings
    if (!mLastFormattedName.equals(pkg.getFormattedName())) {
      return false;
    }

    if (mLastDate != null) {
      if (!mLastDate.equals(pkg.getDate())) {
        return false;
      }
    } else if (pkg.getDate() != null) {
      return false;
    }

    return isEnabled() == pkg.isEnabled();
  }

  @SuppressLint("UseValueOf")
  @SuppressWarnings("UnnecessaryBoxing,BooleanConstructorCall")
  private Boolean getNewBoolean(boolean bool) {
    return new Boolean(bool);
  }
}
