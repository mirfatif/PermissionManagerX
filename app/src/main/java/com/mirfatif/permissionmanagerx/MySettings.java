package com.mirfatif.permissionmanagerx;

import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.text.TextUtils;
import androidx.preference.PreferenceManager;
import androidx.room.Room;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.permsdb.PermissionDao;
import com.mirfatif.permissionmanagerx.permsdb.PermissionDatabase;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

public class MySettings {

  static final String TAG = "MySettings";

  private static MySettings mMySettings;

  public static synchronized MySettings getInstance() {
    if (mMySettings == null) mMySettings = new MySettings();
    return mMySettings;
  }

  private MySettings() {}

  private SharedPreferences mPrefs;

  void initializeVariables() {
    mPrefs = PreferenceManager.getDefaultSharedPreferences(App.getContext());
    mExcludedAppsPrefKey = getString(R.string.filter_settings_excluded_apps_key);
    mExcludedPermsPrefKey = getString(R.string.filter_settings_excluded_perms_key);
    mExtraAppOpsPrefKey = getString(R.string.filter_settings_extra_appops_key);
  }

  public boolean mPrivDaemonAlive = false;
  public boolean mDoRepeatUpdates = true;

  Boolean doLogging = false;
  public boolean DEBUG = false;

  boolean getBoolPref(int keyResId) {
    String prefKey = getString(keyResId);
    int boolKeyId = Utils.getIntField(prefKey + "_default", R.bool.class, TAG);
    return mPrefs.getBoolean(prefKey, App.getContext().getResources().getBoolean(boolKeyId));
  }

  Set<String> getSetPref(int keyId) {
    return mPrefs.getStringSet(getString(keyId), null);
  }

  private String getString(int keyResId) {
    return App.getContext().getString(keyResId);
  }

  void savePref(int key, boolean bool) {
    mPrefs.edit().putBoolean(getString(key), bool).apply();
  }

  void savePref(int key, Set<String> stringSet) {
    mPrefs.edit().putStringSet(getString(key), stringSet).apply();
    updateList(getString(key));
  }

  int getDaemonUid() {
    return mPrefs.getInt(getString(R.string.main_settings_daemon_uid_key), 1000);
  }

  void setDaemonUid(int uid) {
    mPrefs
        .edit()
        .putInt(App.getContext().getString(R.string.main_settings_daemon_uid_key), uid)
        .apply();
  }

  void plusAppLaunchCount() {
    String appLaunchCountKey = getString(R.string.main_settings_app_launch_count_key);
    mPrefs.edit().putInt(appLaunchCountKey, mPrefs.getInt(appLaunchCountKey, 0) + 1).apply();
  }

  boolean shouldNotAskForRating() {
    long lastTS = mPrefs.getLong(getString(R.string.main_settings_ask_for_rating_ts_key), 0);
    if (lastTS == 0) {
      setAskForRatingTs(System.currentTimeMillis() + TimeUnit.DAYS.toMillis(5));
      return true;
    }
    String appLaunchCountKey = getString(R.string.main_settings_app_launch_count_key);
    boolean ask = mPrefs.getInt(appLaunchCountKey, 0) >= 5;
    ask = ask && (System.currentTimeMillis() - lastTS) >= TimeUnit.DAYS.toMillis(5);
    if (ask) {
      mPrefs.edit().putInt(appLaunchCountKey, 0).apply();
      setAskForRatingTs(System.currentTimeMillis());
    }
    return !ask;
  }

  void setAskForRatingTs(long timeStamp) {
    mPrefs
        .edit()
        .putLong(getString(R.string.main_settings_ask_for_rating_ts_key), timeStamp)
        .apply();
  }

  private PermissionDao mPermDb;

  public PermissionDao getPermDb() {
    if (mPermDb == null) {
      Class<PermissionDatabase> dbClass = PermissionDatabase.class;
      String dbName = "permissions.db";
      mPermDb = Room.databaseBuilder(App.getContext(), dbClass, dbName).build().permissionDao();
    }
    return mPermDb;
  }

  public String mQueryText;

  public boolean isSearching() {
    return !TextUtils.isEmpty(mQueryText);
  }

  public boolean isDeepSearchEnabled() {
    return getBoolPref(R.string.main_settings_deep_search_key);
  }

  boolean isCaseSensitiveSearch() {
    return getBoolPref(R.string.main_settings_case_sensitive_search_key);
  }

  boolean isRootGranted() {
    return getBoolPref(R.string.main_settings_root_granted_key);
  }

  boolean isAdbConnected() {
    return getBoolPref(R.string.main_settings_adb_connected_key);
  }

  private List<String> mCriticalApps;

  boolean isCriticalApp(String packageName) {
    if (mCriticalApps == null) {
      mCriticalApps =
          Arrays.asList(App.getContext().getResources().getStringArray(R.array.critical_apps));
    }
    return mCriticalApps.contains(packageName);
  }

  // apps
  public boolean excludeNoIconApps() {
    return getBoolPref(R.string.filter_settings_exclude_no_icon_apps_key);
  }

  public boolean excludeUserApps() {
    return getBoolPref(R.string.filter_settings_exclude_user_apps_key);
  }

  public boolean excludeSystemApps() {
    return getBoolPref(R.string.filter_settings_exclude_system_apps_key);
  }

  public boolean excludeFrameworkApps() {
    return getBoolPref(R.string.filter_settings_exclude_framework_apps_key);
  }

  public boolean excludeDisabledApps() {
    return getBoolPref(R.string.filter_settings_exclude_disabled_apps_key);
  }

  public boolean excludeNoPermissionsApps() {
    return getBoolPref(R.string.filter_settings_exclude_no_perms_apps_key);
  }

  // permissions
  public boolean excludeInvalidPermissions() {
    return getBoolPref(R.string.filter_settings_exclude_invalid_perms_key);
  }

  public boolean excludeNotChangeablePerms() {
    return getBoolPref(R.string.filter_settings_exclude_not_changeable_perms_key);
  }

  public boolean excludeNotGrantedPerms() {
    return getBoolPref(R.string.filter_settings_exclude_not_granted_perms_key);
  }

  public boolean excludeNormalPerms() {
    return getBoolPref(R.string.filter_settings_exclude_normal_perms_key);
  }

  public boolean excludeDangerousPerms() {
    return getBoolPref(R.string.filter_settings_exclude_dangerous_perms_key);
  }

  public boolean excludeSignaturePerms() {
    return getBoolPref(R.string.filter_settings_exclude_signature_perms_key);
  }

  public boolean excludePrivilegedPerms() {
    return getBoolPref(R.string.filter_settings_exclude_privileged_perms_key);
  }

  public boolean excludeAppOpsPerms() {
    return getBoolPref(R.string.filter_settings_exclude_appops_perms_key);
  }

  public boolean excludeNotSetAppOps() {
    return getBoolPref(R.string.filter_settings_exclude_not_set_appops_key);
  }

  public boolean canReadAppOps() {
    return useHiddenAPIs() || mPrivDaemonAlive;
  }

  boolean isAppOpsGranted() {
    return App.getContext().checkSelfPermission(MainActivity.APP_OPS_PERM)
        == PackageManager.PERMISSION_GRANTED;
  }

  private List<String> mAppOpsList;

  public List<String> getAppOpsList() {
    if (mAppOpsList == null) {
      mAppOpsList = PackageParser.getInstance().buildAppOpsList();
    }
    if (mAppOpsList == null) return new ArrayList<>();
    return mAppOpsList;
  }

  CharSequence[] getAppOpsListSorted() {
    List<String> list = new ArrayList<>(getAppOpsList());
    Collections.sort(list);
    return list.toArray(new CharSequence[0]);
  }

  private List<String> mAppOpsModes;

  public List<String> getAppOpsModes() {
    if (mAppOpsModes == null) {
      mAppOpsModes = PackageParser.getInstance().buildAppOpsModes();
    }
    return mAppOpsModes;
  }

  boolean mHiddenAPIsWorking = true;

  public boolean useHiddenAPIs() {
    return getBoolPref(R.string.main_settings_use_hidden_apis_key)
        && mHiddenAPIsWorking
        && isAppOpsGranted();
  }

  private String mExcludedAppsPrefKey;
  private Set<String> mExcludedApps;
  private CharSequence[] mExcludedAppsLabels;
  private CharSequence[] mExcludedAppsNames;

  CharSequence[] getExcludedAppsLabels() {
    if (mExcludedAppsLabels == null) {
      populateExcludedAppsList(false);
    }
    return mExcludedAppsLabels;
  }

  CharSequence[] getExcludedAppsNames() {
    if (mExcludedAppsNames == null) {
      populateExcludedAppsList(false);
    }
    return mExcludedAppsNames;
  }

  Set<String> getExcludedAppsSet() {
    if (mExcludedApps == null) {
      populateExcludedAppsList(false);
    }
    return mExcludedApps;
  }

  int getExcludedAppsCount() {
    return getExcludedAppsSet().size();
  }

  public boolean isPkgExcluded(String packageName) {
    return getExcludedAppsSet().contains(packageName);
  }

  synchronized void populateExcludedAppsList(boolean loadDefaults) {
    if (DEBUG) Utils.debugLog("populateExcludedAppsList", "loadDefaults: " + loadDefaults);

    // on first run or after "reset to defaults" it returns null, so use default values
    Set<String> savedExcludedApps = mPrefs.getStringSet(mExcludedAppsPrefKey, null);
    Set<String> excludedApps = savedExcludedApps;
    if (savedExcludedApps == null || loadDefaults) {
      String[] defaultExcludedApps =
          App.getContext().getResources().getStringArray(R.array.excluded_apps);
      excludedApps = new HashSet<>(Arrays.asList(defaultExcludedApps));
    }

    // let's remove uninstalled packages from excluded apps list
    // also get sorted lists of labels (to show) vs. names (to save)
    // Set (is not ordered and thus) cannot be sorted
    List<Pair> excludedAppsPairList = new ArrayList<>();
    PackageManager packageManager = App.getContext().getPackageManager();

    for (String packageName : excludedApps) {
      String packageLabel;
      try {
        packageLabel =
            packageManager.getApplicationInfo(packageName, 0).loadLabel(packageManager).toString();
      } catch (NameNotFoundException e) {
        // package is not installed
        continue;
      }
      if (packageLabel.equals(packageName)) {
        excludedAppsPairList.add(new Pair(packageLabel, packageName));
      } else {
        excludedAppsPairList.add(new Pair(packageLabel + "\n(" + packageName + ")", packageName));
      }
    }

    // List can be sorted now
    excludedAppsPairList.sort(Comparator.comparing(o -> o.packageLabel.toUpperCase()));

    // separate the pair elements to sorted ordered lists
    CharSequence[] excludedAppsLabels = new CharSequence[excludedAppsPairList.size()];
    CharSequence[] excludedAppsNames = new CharSequence[excludedAppsPairList.size()];
    mExcludedApps = new HashSet<>();

    for (int i = 0; i < excludedAppsPairList.size(); i++) {
      excludedAppsLabels[i] = excludedAppsPairList.get(i).packageLabel;
      excludedAppsNames[i] = excludedAppsPairList.get(i).packageName;
      mExcludedApps.add(excludedAppsPairList.get(i).packageName);
    }

    mExcludedAppsLabels = excludedAppsLabels;
    mExcludedAppsNames = excludedAppsNames;

    // remove uninstalled excluded apps from saved list
    // save preferences only on app's first run or if changed, otherwise
    // OnSharedPreferenceChangeListener will cause infinite loop
    if (savedExcludedApps == null || !savedExcludedApps.equals(mExcludedApps)) {
      mPrefs.edit().putStringSet(mExcludedAppsPrefKey, new HashSet<>(mExcludedApps)).apply();
    }
  }

  private String mExcludedPermsPrefKey;
  private Set<String> mExcludedPerms;
  private CharSequence[] mExcludedPermsNames;

  CharSequence[] getExcludedPermsNames() {
    if (mExcludedPermsNames == null) {
      populateExcludedPermsList();
    }
    return mExcludedPermsNames;
  }

  private Set<String> getExcludedPerms() {
    if (mExcludedPerms == null) {
      populateExcludedPermsList();
    }
    return mExcludedPerms;
  }

  int getExcludedPermsCount() {
    return getExcludedPerms().size();
  }

  public boolean isPermExcluded(String permissionName) {
    return getExcludedPerms().contains(permissionName);
  }

  synchronized void populateExcludedPermsList() {
    if (DEBUG) Utils.debugLog("populateExcludedPermsList", "Called");
    Set<String> excludedPerms = mPrefs.getStringSet(mExcludedPermsPrefKey, null);
    if (excludedPerms == null) excludedPerms = new HashSet<>();

    List<String> excludedPermsList = new ArrayList<>(excludedPerms);
    Collections.sort(excludedPermsList);

    mExcludedPerms = new HashSet<>(excludedPermsList);
    mExcludedPermsNames = excludedPermsList.toArray(new CharSequence[0]);
  }

  private String mExtraAppOpsPrefKey;
  private Set<String> mExtraAppOps;

  public Set<String> getExtraAppOps() {
    if (mExtraAppOps == null) {
      populateExtraAppOpsList(false);
    }
    return mExtraAppOps;
  }

  int getExtraAppOpsCount() {
    return getExtraAppOps().size();
  }

  public boolean isExtraAppOp(String opName) {
    return getExtraAppOps().contains(opName);
  }

  synchronized void populateExtraAppOpsList(boolean loadDefaults) {
    if (DEBUG) Utils.debugLog("populateExtraAppOpsList", "loadDefaults: " + loadDefaults);
    // on first run or after "reset to defaults" it returns null, so use default values
    Set<String> savedExtraAppOps = mPrefs.getStringSet(mExtraAppOpsPrefKey, null);
    Set<String> extraAppOps = savedExtraAppOps;
    if (savedExtraAppOps == null || loadDefaults) {
      String[] defaultExtraAppOps =
          App.getContext().getResources().getStringArray(R.array.extra_app_ops);
      extraAppOps = new HashSet<>(Arrays.asList(defaultExtraAppOps));
    }

    // let's remove AppOps not on this Android version
    mExtraAppOps = new HashSet<>();
    for (String extraAppOp : extraAppOps) {
      // Necessarily build mExtraAppOps here even with excludeAppOpsPerms(). Otherwise on unchecking
      // "Exclude AppOps" the immediate call to getExtraAppOps() from FilterSettingsFragment
      // receives an empty value which clears previous saved values.
      if (getAppOpsList().size() == 0 || getAppOpsList().contains(extraAppOp)) {
        mExtraAppOps.add(extraAppOp);
      }
    }

    // Remove invalid AppOps from saved list.
    // Save preferences only on app's first run or if changed, otherwise
    // OnSharedPreferenceChangeListener will cause infinite loop.
    if (savedExtraAppOps == null || !savedExtraAppOps.equals(mExtraAppOps)) {
      mPrefs.edit().putStringSet(mExtraAppOpsPrefKey, new HashSet<>(mExtraAppOps)).apply();
    }
  }

  synchronized void updateList(String key) {
    if (key.equals(mExcludedAppsPrefKey)) populateExcludedAppsList(false);
    else if (key.equals(mExcludedPermsPrefKey)) populateExcludedPermsList();
    else if (key.equals(mExtraAppOpsPrefKey)) populateExtraAppOpsList(false);
  }

  void resetToDefaults() {
    if (DEBUG) Utils.debugLog("MySettings", "resetToDefaults() called");
    // excluded apps Set must be explicitly removed to set default values
    // .clear() does not work correctly
    mPrefs
        .edit()
        .remove(getString(R.string.filter_settings_exclude_no_icon_apps_key))
        .remove(getString(R.string.filter_settings_exclude_user_apps_key))
        .remove(getString(R.string.filter_settings_exclude_system_apps_key))
        .remove(getString(R.string.filter_settings_exclude_framework_apps_key))
        .remove(getString(R.string.filter_settings_exclude_disabled_apps_key))
        .remove(getString(R.string.filter_settings_excluded_apps_key))
        .remove(getString(R.string.filter_settings_exclude_no_perms_apps_key))
        .remove(getString(R.string.filter_settings_exclude_invalid_perms_key))
        .remove(getString(R.string.filter_settings_exclude_not_changeable_perms_key))
        .remove(getString(R.string.filter_settings_exclude_not_granted_perms_key))
        .remove(getString(R.string.filter_settings_exclude_normal_perms_key))
        .remove(getString(R.string.filter_settings_exclude_dangerous_perms_key))
        .remove(getString(R.string.filter_settings_exclude_signature_perms_key))
        .remove(getString(R.string.filter_settings_exclude_privileged_perms_key))
        .remove(getString(R.string.filter_settings_exclude_appops_perms_key))
        .remove(getString(R.string.filter_settings_exclude_not_set_appops_key))
        .remove(getString(R.string.filter_settings_excluded_perms_key))
        .remove(getString(R.string.filter_settings_extra_appops_key))
        .apply();

    // StringSet is not cleared (null) by remove() or clear() sometimes.
    populateExcludedAppsList(true);
    populateExtraAppOpsList(true);
  }

  private static class Pair {
    String packageLabel;
    String packageName;

    Pair(String packageLabel, String packageName) {
      this.packageLabel = packageLabel;
      this.packageName = packageName;
    }
  }
}
