package com.mirfatif.permissionmanagerx.prefs;

import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.SystemClock;
import android.text.TextUtils;
import android.util.Log;
import androidx.room.Room;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.annot.SecurityLibBug;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.main.MainActivity;
import com.mirfatif.permissionmanagerx.parser.AppOpsParser;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionDao;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionDatabase;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Util;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

public class MySettings {

  private static final String TAG = "MySettings";

  private static MySettings mMySettings;

  public static synchronized MySettings getInstance() {
    if (mMySettings == null) {
      mMySettings = new MySettings();
    }
    return mMySettings;
  }

  private final SharedPreferences mPrefs;

  // Removed mEncPrefs and mUseHiddenAPIs initialization due to bug
  @SecurityLibBug
  private MySettings() {
    mPrefs = Utils.getDefPrefs();
    mExcludedAppsPrefKey = getString(R.string.pref_filter_excluded_apps_key);
    mExcludedPermsPrefKey = getString(R.string.pref_filter_excluded_perms_key);
    mExtraAppOpsPrefKey = getString(R.string.pref_filter_extra_appops_key);
  }

  private boolean mPrivDaemonAlive = false;

  public boolean isPrivDaemonAlive() {
    return mPrivDaemonAlive;
  }

  public void setPrivDaemonAlive(boolean alive) {
    mPrivDaemonAlive = alive;
  }

  public boolean shouldRemindMissingPrivileges() {
    return getBoolPref(R.string.pref_settings_privileges_reminder_key);
  }

  public void setPrivReminderOff() {
    savePref(R.string.pref_settings_privileges_reminder_key, false);
  }

  private boolean DEBUG = false;
  private boolean daemonLogging = false;

  public boolean isDebug() {
    return DEBUG;
  }

  public void setDebugLog(boolean debugLog) {
    DEBUG = daemonLogging = debugLog;
  }

  public boolean shouldStartDaemonLog() {
    if (daemonLogging) {
      daemonLogging = false;
      return true;
    }
    return false;
  }

  public boolean getBoolPref(int keyResId) {
    String prefKey = getString(keyResId);
    Integer boolKeyId =
        Utils.getStaticIntField(prefKey + "_default", R.bool.class, TAG + ": getBoolPref()");
    if (boolKeyId == null) {
      return false;
    }
    if (prefKey.endsWith("_enc")) {
      return Utils.getEncPrefs()
          .getBoolean(prefKey, App.getContext().getResources().getBoolean(boolKeyId));
    } else {
      return mPrefs.getBoolean(prefKey, App.getContext().getResources().getBoolean(boolKeyId));
    }
  }

  public int getIntPref(int keyResId) {
    String prefKey = getString(keyResId);
    Integer intKeyId =
        Utils.getStaticIntField(prefKey + "_default", R.integer.class, TAG + ": getIntPref()");
    if (intKeyId == null) {
      return -1;
    }
    if (prefKey.endsWith("_enc")) {
      return Utils.getEncPrefs()
          .getInt(prefKey, App.getContext().getResources().getInteger(intKeyId));
    } else {
      return mPrefs.getInt(prefKey, App.getContext().getResources().getInteger(intKeyId));
    }
  }

  private long getLongPref(int keyResId) {
    String prefKey = getString(keyResId);
    if (prefKey.endsWith("_enc")) {
      return Utils.getEncPrefs().getLong(prefKey, 0);
    } else {
      return mPrefs.getLong(prefKey, 0);
    }
  }

  private Set<String> getSetPref(int keyId) {
    return mPrefs.getStringSet(getString(keyId), null);
  }

  private String getString(int keyResId) {
    return App.getContext().getString(keyResId);
  }

  public void savePref(int key, boolean bool) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      Utils.getEncPrefs().edit().putBoolean(prefKey, bool).apply();
    } else {
      mPrefs.edit().putBoolean(prefKey, bool).apply();
    }
  }

  public void savePref(int key, int integer) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      Utils.getEncPrefs().edit().putInt(prefKey, integer).apply();
    } else {
      mPrefs.edit().putInt(prefKey, integer).apply();
    }
  }

  private void savePref(int key, long _long) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      Utils.getEncPrefs().edit().putLong(prefKey, _long).apply();
    } else {
      mPrefs.edit().putLong(prefKey, _long).apply();
    }
  }

  private void savePref(int key, Set<String> stringSet) {
    mPrefs.edit().putStringSet(getString(key), stringSet).apply();
    updateList(getString(key));
  }

  public int getAdbPort() {
    return getIntPref(R.string.pref_main_adb_port_key);
  }

  public void setAdbPort(int port) {
    savePref(R.string.pref_main_adb_port_key, port);
  }

  public int getDaemonUid() {
    return getIntPref(R.string.pref_main_daemon_uid_key);
  }

  public void setDaemonUid(int uid) {
    savePref(R.string.pref_main_daemon_uid_key, uid);
  }

  public static String CONTEXT_DEFAULT = "default";
  public static String CONTEXT_SHELL = "u:r:shell:s0";

  public String getDaemonContext() {
    return mPrefs.getString(getString(R.string.pref_main_daemon_context_key), CONTEXT_SHELL);
  }

  public void setDaemonContext(String context) {
    mPrefs.edit().putString(getString(R.string.pref_main_daemon_context_key), context).apply();
  }

  public boolean dexInTmpDir() {
    return getBoolPref(R.string.pref_main_daemon_tmp_dir_key);
  }

  public void setDexInTmpDir(boolean dexInTmpDir) {
    savePref(R.string.pref_main_daemon_tmp_dir_key, dexInTmpDir);
  }

  public boolean shouldCheckForUpdates() {
    if (!getBoolPref(R.string.pref_settings_check_for_updates_key)) {
      return false;
    }
    long lastTS = getLongPref(R.string.pref_settings_check_for_updates_ts_enc_key);
    return (System.currentTimeMillis() - lastTS) >= TimeUnit.DAYS.toMillis(1);
  }

  public void setCheckForUpdatesTs(long timeStamp) {
    savePref(R.string.pref_settings_check_for_updates_ts_enc_key, timeStamp);
  }

  @SecurityLibBug
  public void plusAppLaunchCount() {
    int appLaunchCountId = R.string.pref_main_app_launch_count_enc_key;
    Utils.runInBg(() -> savePref(appLaunchCountId, getIntPref(appLaunchCountId) + 1));
  }

  public boolean shouldAskToSendCrashReport() {
    int crashCount = getIntPref(R.string.pref_main_crash_report_count_enc_key);
    long lastTS = getLongPref(R.string.pref_main_crash_report_ts_enc_key);
    long currTime = System.currentTimeMillis();

    if (crashCount >= 5 || (currTime - lastTS) >= TimeUnit.DAYS.toMillis(1)) {
      savePref(R.string.pref_main_crash_report_ts_enc_key, currTime);
      savePref(R.string.pref_main_crash_report_count_enc_key, 1);
      return true;
    }

    savePref(R.string.pref_main_crash_report_count_enc_key, crashCount + 1);
    return false;
  }

  // Do not ask for feedback before the first package loading is completed
  private boolean mMayAskForFeedback = false;

  public void setMayAskForFeedback() {
    Utils.runInBg(
        () -> {
          SystemClock.sleep(5000);
          mMayAskForFeedback = true;
        });
  }

  public boolean shouldAskForFeedback() {
    if (!mMayAskForFeedback) {
      return false;
    }
    long lastTS = getLongPref(R.string.pref_main_ask_for_feedback_ts_enc_key);
    if (lastTS == 0) {
      setAskForFeedbackTs(System.currentTimeMillis() + TimeUnit.DAYS.toMillis(5));
      return false;
    }
    int appLaunchCountId = R.string.pref_main_app_launch_count_enc_key;
    boolean ask = getIntPref(appLaunchCountId) >= 5;
    ask = ask && (System.currentTimeMillis() - lastTS) >= TimeUnit.DAYS.toMillis(5);
    if (ask) {
      savePref(appLaunchCountId, 0);
      setAskForFeedbackTs(System.currentTimeMillis());
    }
    return ask;
  }

  public void setAskForFeedbackTs(long timeStamp) {
    savePref(R.string.pref_main_ask_for_feedback_ts_enc_key, timeStamp);
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

  private String mQueryText;

  public String getQueryText() {
    return mQueryText;
  }

  public void setQueryText(String queryText) {
    mQueryText = queryText;
  }

  public boolean isSearching() {
    return !TextUtils.isEmpty(mQueryText);
  }

  public boolean isDeepSearching() {
    return isSearching() && isDeepSearchEnabled();
  }

  public boolean isDeepSearchEnabled() {
    return getBoolPref(R.string.pref_main_deep_search_enc_key);
  }

  public void setDeepSearchEnabled(boolean enabled) {
    savePref(R.string.pref_main_deep_search_enc_key, enabled);
  }

  public boolean isCaseSensitiveSearch() {
    return getBoolPref(R.string.pref_main_case_sensitive_search_enc_key);
  }

  public void setCaseSensitiveSearch(boolean isSensitive) {
    savePref(R.string.pref_main_case_sensitive_search_enc_key, isSensitive);
  }

  public boolean isQuickScanEnabled() {
    return getBoolPref(R.string.pref_settings_quick_scan_key);
  }

  public boolean shouldDoQuickScan() {
    return isQuickScanEnabled() && !isDeepSearching();
  }

  public boolean shouldShowRefs() {
    return !isQuickScanEnabled() && !isDeepSearching();
  }

  public boolean isRootGranted() {
    return getBoolPref(R.string.pref_main_root_granted_enc_key);
  }

  public void setRootGranted(boolean granted) {
    savePref(R.string.pref_main_root_granted_enc_key, granted);
  }

  public boolean isAdbConnected() {
    return getBoolPref(R.string.pref_main_adb_connected_enc_key);
  }

  public void setAdbConnected(boolean connected) {
    savePref(R.string.pref_main_adb_connected_enc_key, connected);
  }

  private List<String> mCriticalApps;

  public boolean isCriticalApp(String packageName) {
    if (mCriticalApps == null) {
      mCriticalApps =
          Arrays.asList(App.getContext().getResources().getStringArray(R.array.critical_apps));
    }
    return mCriticalApps.contains(packageName);
  }

  // apps
  public boolean excludeNoIconApps() {
    return getBoolPref(R.string.pref_filter_exclude_no_icon_apps_key);
  }

  public boolean excludeUserApps() {
    return getBoolPref(R.string.pref_filter_exclude_user_apps_key);
  }

  public boolean excludeSystemApps() {
    return getBoolPref(R.string.pref_filter_exclude_system_apps_key);
  }

  public boolean excludeFrameworkApps() {
    return getBoolPref(R.string.pref_filter_exclude_framework_apps_key);
  }

  public boolean excludeDisabledApps() {
    return getBoolPref(R.string.pref_filter_exclude_disabled_apps_key);
  }

  public boolean excludeNoPermissionsApps() {
    return getBoolPref(R.string.pref_filter_exclude_no_perms_apps_key);
  }

  public boolean shouldExcludeNoPermApps() {
    return !isQuickScanEnabled() && excludeNoPermissionsApps();
  }

  // permissions
  public boolean excludeInvalidPermissions() {
    return getBoolPref(R.string.pref_filter_exclude_invalid_perms_key);
  }

  public boolean excludeNotChangeablePerms() {
    return getBoolPref(R.string.pref_filter_exclude_not_changeable_perms_key);
  }

  public boolean excludeNotGrantedPerms() {
    return getBoolPref(R.string.pref_filter_exclude_not_granted_perms_key);
  }

  public boolean excludeNormalPerms() {
    return getBoolPref(R.string.pref_filter_exclude_normal_perms_key);
  }

  public boolean excludeDangerousPerms() {
    return getBoolPref(R.string.pref_filter_exclude_dangerous_perms_key);
  }

  public boolean excludeSignaturePerms() {
    return getBoolPref(R.string.pref_filter_exclude_signature_perms_key);
  }

  public boolean excludePrivilegedPerms() {
    return getBoolPref(R.string.pref_filter_exclude_privileged_perms_key);
  }

  public boolean excludeAppOpsPerms() {
    return getBoolPref(R.string.pref_filter_exclude_appops_perms_key);
  }

  public boolean excludeNotSetAppOps() {
    return getBoolPref(R.string.pref_filter_exclude_not_set_appops_key);
  }

  public boolean canReadAppOps() {
    return canUseHiddenAPIs() || mPrivDaemonAlive;
  }

  public boolean isAppOpsGranted() {
    return App.getContext().checkSelfPermission(MainActivity.APP_OPS_PERM)
        == PackageManager.PERMISSION_GRANTED;
  }

  // Do not sort this list since the position is the AppOps code
  private List<String> mAppOpsList;

  public List<String> getAppOpsList() {
    if (mAppOpsList == null) {
      mAppOpsList = AppOpsParser.getInstance().buildAppOpsList();
    }
    if (mAppOpsList == null) {
      return new ArrayList<>();
    }
    return mAppOpsList;
  }

  private List<String> mAppOpsModes;

  public List<String> getAppOpsModes() {
    if (mAppOpsModes == null) {
      mAppOpsModes = AppOpsParser.getInstance().buildAppOpsModes();
    }
    return mAppOpsModes;
  }

  public boolean canUseHiddenAPIs() {
    return useHiddenAPIs() && isAppOpsGranted();
  }

  // Accessing Preference every time may slow down
  private Boolean mUseHiddenAPIs = null;

  public boolean useHiddenAPIs() {
    if (mUseHiddenAPIs == null) {
      mUseHiddenAPIs = getBoolPref(R.string.pref_main_use_hidden_apis_enc_key);
    }
    return mUseHiddenAPIs;
  }

  public void setUseHiddenAPIs(boolean useHiddenAPIs) {
    mUseHiddenAPIs = useHiddenAPIs;
    savePref(R.string.pref_main_use_hidden_apis_enc_key, useHiddenAPIs);
  }

  public boolean forceDarkMode() {
    return getBoolPref(R.string.pref_main_dark_theme_key);
  }

  public void setForceDarkMode(boolean force) {
    savePref(R.string.pref_main_dark_theme_key, force);
  }

  public boolean useSocket() {
    return getBoolPref(R.string.pref_main_use_socket_enc_key);
  }

  public void setUseSocket(boolean useSocket) {
    savePref(R.string.pref_main_use_socket_enc_key, useSocket);
  }

  private final String mExcludedAppsPrefKey;
  private Set<String> mExcludedApps;
  private CharSequence[] mExcludedAppsLabels;

  public CharSequence[] getExcludedAppsLabels() {
    if (mExcludedAppsLabels == null) {
      populateExcludedAppsList(false);
    }
    return mExcludedAppsLabels;
  }

  public Set<String> getExcludedApps() {
    if (mExcludedApps == null) {
      populateExcludedAppsList(false);
    }
    return mExcludedApps;
  }

  public int getExcludedAppsCount() {
    return getExcludedApps().size();
  }

  public boolean isPkgExcluded(String packageName) {
    return getExcludedApps().contains(packageName);
  }

  private final ReentrantLock EXCLUDED_APPS_LOCK = new ReentrantLock();

  public void getExcludedAppsLock() {
    EXCLUDED_APPS_LOCK.lock();
  }

  public void releaseExcludedAppsLock() {
    EXCLUDED_APPS_LOCK.unlock();
  }

  public void populateExcludedAppsList(boolean loadDefaults) {
    getExcludedAppsLock();
    if (DEBUG) {
      Util.debugLog(TAG, "populateExcludedAppsList(): loadDefaults: " + loadDefaults);
    }

    // on first run or after "reset to defaults" it returns null, so use default values
    Set<String> savedExcludedApps = mPrefs.getStringSet(mExcludedAppsPrefKey, null);
    Set<String> excludedApps = savedExcludedApps;
    if (savedExcludedApps == null || loadDefaults) {
      String[] defaultExcludedApps =
          App.getContext().getResources().getStringArray(R.array.excluded_apps);
      excludedApps = new HashSet<>(Arrays.asList(defaultExcludedApps));
    }

    // Let's remove uninstalled packages from excluded apps list.
    // Also get sorted lists of labels (to show) vs. names (to save).
    // Set (is not ordered and thus) cannot be sorted.
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

    // Separate the pair elements to sorted ordered lists.
    CharSequence[] excludedAppsLabels = new CharSequence[excludedAppsPairList.size()];
    mExcludedApps = new LinkedHashSet<>(); // Must be ordered to correspond with Labels

    for (int i = 0; i < excludedAppsPairList.size(); i++) {
      excludedAppsLabels[i] = excludedAppsPairList.get(i).packageLabel;
      mExcludedApps.add(excludedAppsPairList.get(i).packageName);
    }

    mExcludedAppsLabels = excludedAppsLabels;

    // Remove uninstalled excluded apps from saved list.
    // Save preferences only on app's first run or if changed, otherwise
    // OnSharedPreferenceChangeListener will cause infinite loop.
    if (savedExcludedApps == null || !savedExcludedApps.equals(mExcludedApps)) {
      mPrefs.edit().putStringSet(mExcludedAppsPrefKey, new HashSet<>(mExcludedApps)).apply();
    }
    releaseExcludedAppsLock();
  }

  public void clearExcludedAppsList() {
    savePref(R.string.pref_filter_excluded_apps_key, new HashSet<>());
  }

  public void addPkgToExcludedApps(String pkgName) {
    Set<String> excludedApps = getSetPref(R.string.pref_filter_excluded_apps_key);
    if (excludedApps == null) {
      excludedApps = new HashSet<>();
    } else {
      excludedApps = new HashSet<>(excludedApps);
    }
    excludedApps.add(pkgName);
    savePref(R.string.pref_filter_excluded_apps_key, excludedApps);
  }

  private final String mExcludedPermsPrefKey;
  private Set<String> mExcludedPerms;

  public Set<String> getExcludedPerms() {
    if (mExcludedPerms == null) {
      populateExcludedPermsList();
    }
    return mExcludedPerms;
  }

  public int getExcludedPermsCount() {
    return getExcludedPerms().size();
  }

  public boolean isPermExcluded(String permissionName) {
    return getExcludedPerms().contains(permissionName);
  }

  private final ReentrantLock EXCLUDED_PERMS_LOCK = new ReentrantLock();

  public void getExcludedPermsLock() {
    EXCLUDED_PERMS_LOCK.lock();
  }

  public void releaseExcludedPermsLock() {
    EXCLUDED_PERMS_LOCK.unlock();
  }

  public void populateExcludedPermsList() {
    getExcludedPermsLock();
    if (DEBUG) {
      Util.debugLog(TAG, "populateExcludedPermsList() called");
    }
    Set<String> excludedPerms = mPrefs.getStringSet(mExcludedPermsPrefKey, null);
    if (excludedPerms == null) {
      excludedPerms = new HashSet<>();
    }

    List<String> excludedPermsList = new ArrayList<>(excludedPerms);
    excludedPermsList.sort(Comparator.comparing(String::toUpperCase));

    mExcludedPerms = new LinkedHashSet<>(); // Maintain the sort order
    mExcludedPerms.addAll(excludedPermsList);
    releaseExcludedPermsLock();
  }

  public void clearExcludedPermsList() {
    savePref(R.string.pref_filter_excluded_perms_key, new HashSet<>());
  }

  public void addPermToExcludedPerms(String permName) {
    Set<String> excludedPerms = getSetPref(R.string.pref_filter_excluded_perms_key);
    if (excludedPerms == null) {
      excludedPerms = new HashSet<>();
    } else {
      excludedPerms = new HashSet<>(excludedPerms);
    }
    excludedPerms.add(permName);
    savePref(R.string.pref_filter_excluded_perms_key, excludedPerms);
  }

  private final String mExtraAppOpsPrefKey;
  private Set<String> mExtraAppOps;

  public Set<String> getExtraAppOps() {
    if (mExtraAppOps == null) {
      populateExtraAppOpsList(false);
    }
    return mExtraAppOps;
  }

  public int getExtraAppOpsCount() {
    return getExtraAppOps().size();
  }

  public boolean isExtraAppOp(String opName) {
    return getExtraAppOps().contains(opName);
  }

  private final ReentrantLock EXTRA_APP_OPS_LOCK = new ReentrantLock();

  public void getExtraAppOpsLock() {
    EXTRA_APP_OPS_LOCK.lock();
  }

  public void releaseExtraAppOpsLock() {
    EXTRA_APP_OPS_LOCK.unlock();
  }

  public void populateExtraAppOpsList(boolean loadDefaults) {
    getExtraAppOpsLock();
    if (DEBUG) {
      Util.debugLog(TAG, "populateExtraAppOpsList(): loadDefaults: " + loadDefaults);
    }
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
      // Necessarily build mExtraAppOps here even with excludeAppOpsPerms(). Otherwise on
      // unchecking
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
    releaseExtraAppOpsLock();
  }

  public void clearExtraAppOpsList() {
    savePref(R.string.pref_filter_extra_appops_key, new HashSet<>());
  }

  public void updateList(String key) {
    if (key.equals(mExcludedAppsPrefKey)) {
      populateExcludedAppsList(false);
    } else if (key.equals(mExcludedPermsPrefKey)) {
      populateExcludedPermsList();
    } else if (key.equals(mExtraAppOpsPrefKey)) {
      populateExtraAppOpsList(false);
    }
  }

  public void resetToDefaults() {
    // excluded apps Set must be explicitly removed to set default values
    // .clear() does not work correctly
    Editor prefEditor = mPrefs.edit();
    int count = 0;
    for (Field field : R.string.class.getDeclaredFields()) {
      String strName = field.getName();
      if (strName.startsWith("pref_filter_") && strName.endsWith("_key")) {
        Integer strKeyResId =
            Utils.getStaticIntField(strName, R.string.class, TAG + ": resetToDefaults()");
        if (strKeyResId == null) {
          continue;
        }
        String prefKey = getString(strKeyResId);
        prefEditor.remove(prefKey);
        count++;
      }
    }
    prefEditor.apply();
    Log.i(TAG, "resetToDefaults(): " + count + " preferences removed");

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
