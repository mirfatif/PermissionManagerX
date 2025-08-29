package com.mirfatif.permissionmanagerx.prefs;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.os.Build;
import android.text.TextUtils;
import android.text.format.DateUtils;
import androidx.preference.PreferenceManager;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveEvent;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

public enum MySettings {
  INS;

  private boolean mDebug;

  public boolean isDebug() {
    return mDebug;
  }

  public void setDebugLog(boolean debugLog) {
    mDebug = debugLog;
  }

  public static SharedPreferences getDefPrefs() {
    return PreferenceManager.getDefaultSharedPreferences(App.getCxt());
  }

  public static SharedPreferences getNoBackupPrefs() {
    return MySettingsFlavor.INS.getNoBackupPrefs();
  }

  public boolean getBoolPref(int key, int def) {
    return getBoolPref(key, App.getRes().getBoolean(def));
  }

  public boolean getBoolPref(int key, boolean def) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      return getNoBackupPrefs().getBoolean(prefKey, def);
    } else {
      return getDefPrefs().getBoolean(prefKey, def);
    }
  }

  public int getIntPref(int key, int def) {
    return getIntegerPref(key, App.getCxt().getResources().getInteger(def));
  }

  public int getIntegerPref(int key, int defValue) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      return getNoBackupPrefs().getInt(prefKey, defValue);
    } else {
      return getDefPrefs().getInt(prefKey, defValue);
    }
  }

  private long getLongPref(int key) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      return getNoBackupPrefs().getLong(prefKey, 0);
    } else {
      return getDefPrefs().getLong(prefKey, 0);
    }
  }

  public String getStringPref(int key, int def) {
    return getStringPref(key, def == 0 ? null : getString(def));
  }

  public String getStringPref(int key, String def) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      return getNoBackupPrefs().getString(prefKey, def);
    } else {
      return getDefPrefs().getString(prefKey, def);
    }
  }

  public Set<String> getSetPref(int key) {
    return getDefPrefs().getStringSet(getString(key), null);
  }

  public void savePref(int key, boolean val) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      getNoBackupPrefs().edit().putBoolean(prefKey, val).apply();
    } else {
      getDefPrefs().edit().putBoolean(prefKey, val).apply();
    }
  }

  public void savePref(int key, int val) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      getNoBackupPrefs().edit().putInt(prefKey, val).apply();
    } else {
      getDefPrefs().edit().putInt(prefKey, val).apply();
    }
  }

  private void savePref(int key, long val) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      getNoBackupPrefs().edit().putLong(prefKey, val).apply();
    } else {
      getDefPrefs().edit().putLong(prefKey, val).apply();
    }
  }

  public void savePref(int key, String val) {
    String prefKey = getString(key);
    if (prefKey.endsWith("_enc")) {
      getNoBackupPrefs().edit().putString(prefKey, val).apply();
    } else {
      getDefPrefs().edit().putString(prefKey, val).apply();
    }
  }

  public String getDarkThemeMode() {
    try {
      return getStringPref(
          R.string.pref_settings_dark_theme_key, R.string.pref_settings_dark_theme_default);
    } catch (ClassCastException e) {
      getDefPrefs().edit().remove(getString(R.string.pref_settings_dark_theme_key)).apply();
      return getDarkThemeMode();
    }
  }

  public boolean shouldRemindMissingPrivileges() {
    return getBoolPref(
        R.string.pref_settings_privileges_reminder_key,
        R.bool.pref_settings_privileges_reminder_default);
  }

  public void setPrivReminderOff() {
    savePref(R.string.pref_settings_privileges_reminder_key, false);
  }

  public String getLocale() {
    return getStringPref(R.string.pref_settings_locale_key, R.string.pref_settings_locale_default);
  }

  public boolean shouldCheckForUpdates() {
    if (!getBoolPref(
        R.string.pref_settings_check_for_updates_key,
        R.bool.pref_settings_check_for_updates_default)) {
      return false;
    }
    long lastTS = getLongPref(R.string.pref_settings_check_for_updates_ts_enc_key);
    return (System.currentTimeMillis() - lastTS) >= TimeUnit.DAYS.toMillis(1);
  }

  public void setCheckForUpdatesTs(long timeStamp) {
    savePref(R.string.pref_settings_check_for_updates_ts_enc_key, timeStamp);
  }

  public boolean getShowUnsupportedSdkWarning() {
    int warnedSdk = getIntegerPref(R.string.pref_main_warned_unsupported_sdk_enc_key, 0);
    return Build.VERSION.SDK_INT > warnedSdk
        && Build.VERSION.SDK_INT > ApiUtils.getMyAppInfo().targetSdkVersion;
  }

  public void onUnsupportedSdkWarningShown() {
    savePref(R.string.pref_main_warned_unsupported_sdk_enc_key, Build.VERSION.SDK_INT);
  }

  public void plusAppLaunchCount() {
    int appLaunchCountId = R.string.pref_main_app_launch_count_enc_key;
    int count = getIntPref(appLaunchCountId, R.integer.pref_main_app_launch_count_enc_default);
    savePref(appLaunchCountId, count + 1);
  }

  private boolean mMayAskForFeedback = false;

  public void setMayAskForFeedback(boolean askForFeedback) {
    mMayAskForFeedback = askForFeedback;
  }

  public boolean shouldAskForFeedback() {
    if (!mMayAskForFeedback) {
      return false;
    }

    long lastTS = getLongPref(R.string.pref_main_ask_for_feedback_ts_enc_key);

    if (lastTS == 0) {
      setAskForFeedbackTs(false);
      savePref(R.string.pref_main_feedback_app_version_enc_key, BuildConfig.VERSION_CODE);
      return false;
    }

    if (getIntegerPref(R.string.pref_main_feedback_app_version_enc_key, 0)
        < BuildConfig.VERSION_CODE) {
      setAskForFeedbackTs(false);
      savePref(R.string.pref_main_feedback_app_version_enc_key, BuildConfig.VERSION_CODE);
      return false;
    }

    if (getIntPref(
            R.string.pref_main_app_launch_count_enc_key,
            R.integer.pref_main_app_launch_count_enc_default)
        < 10) {
      return false;
    }

    return System.currentTimeMillis() >= lastTS;
  }

  public void setAskForFeedbackTs(boolean longTs) {
    savePref(R.string.pref_main_app_launch_count_enc_key, 0);
    savePref(
        R.string.pref_main_ask_for_feedback_ts_enc_key,
        System.currentTimeMillis() + (DateUtils.WEEK_IN_MILLIS * (longTs ? 24 : 2)));
  }

  public boolean shouldGrantAppPrivs() {
    long lastUpdate = ApiUtils.getMyPkgInfo().lastUpdateTime;
    long lastCheck = getLongPref(R.string.pref_privs_app_privs_check_ts_enc_key);
    boolean updated = lastUpdate >= lastCheck;
    if (updated) {
      savePref(R.string.pref_privs_app_privs_check_ts_enc_key, System.currentTimeMillis());
    }
    return updated;
  }

  public boolean shouldAskForNotifPerm() {
    long lastTS = getLongPref(R.string.pref_main_ask_for_notif_perm_ts_enc_key);
    return lastTS == 0 || (System.currentTimeMillis() - lastTS) >= TimeUnit.DAYS.toMillis(30);
  }

  public void setAskForNotifPermTs() {
    savePref(R.string.pref_main_ask_for_notif_perm_ts_enc_key, System.currentTimeMillis());
  }

  public boolean shouldAskToSendCrashReport() {
    int crashCount =
        getIntPref(
            R.string.pref_main_crash_report_count_enc_key,
            R.integer.pref_main_crash_report_count_enc_default);
    long lastTS = getLongPref(R.string.pref_main_crash_report_ts_enc_key);
    long currTime = System.currentTimeMillis();

    Editor prefEditor = getNoBackupPrefs().edit();
    try {
      if (crashCount >= 2 || (currTime - lastTS) >= TimeUnit.DAYS.toMillis(1)) {
        prefEditor.putLong(getString(R.string.pref_main_crash_report_ts_enc_key), currTime);
        prefEditor.putInt(getString(R.string.pref_main_crash_report_count_enc_key), 1);
        return true;
      }
      prefEditor.putInt(getString(R.string.pref_main_crash_report_count_enc_key), crashCount + 1);
    } finally {
      prefEditor.commit();
    }

    return false;
  }

  public boolean shouldClearWebViewCache() {
    long lastUpdate = ApiUtils.getMyPkgInfo().lastUpdateTime;
    long lastCheck = getLongPref(R.string.pref_help_web_view_clear_cache_ts_enc_key);
    boolean updated = lastUpdate >= lastCheck;
    savePref(R.string.pref_help_web_view_clear_cache_ts_enc_key, System.currentTimeMillis());
    return updated;
  }

  public int getHelpFontSize() {
    return getIntPref(R.string.pref_help_font_size_key, R.integer.pref_help_font_size_default);
  }

  public void setHelpFontSize(int size) {
    savePref(R.string.pref_help_font_size_key, size);
  }

  public boolean warnDangerousPkgChanges() {
    return getBoolPref(
        R.string.pref_main_warn_dang_change_enc_key, R.bool.pref_main_warn_dang_change_enc_default);
  }

  public void disableWarnDangerousPkgChanges() {
    savePref(R.string.pref_main_warn_dang_change_enc_key, false);
  }

  public boolean warnDangerousPermChanges() {
    return getBoolPref(
        R.string.pref_package_warn_dang_change_enc_key,
        R.bool.pref_package_warn_dang_change_enc_default);
  }

  public void disableWarnDangerousPermChanges() {
    savePref(R.string.pref_package_warn_dang_change_enc_key, false);
  }

  public boolean isRootEnabled() {
    return getBoolPref(
        R.string.pref_privs_root_enabled_enc_key, R.bool.pref_privs_root_enabled_enc_default);
  }

  public void setRootEnabled(boolean granted) {
    savePref(R.string.pref_privs_root_enabled_enc_key, granted);
    drawerPrefChanged();
  }

  public boolean isAdbEnabled() {
    return getBoolPref(
        R.string.pref_privs_adb_enabled_enc_key, R.bool.pref_privs_adb_enabled_enc_default);
  }

  public void setAdbEnabled(boolean connected) {
    savePref(R.string.pref_privs_adb_enabled_enc_key, connected);
    drawerPrefChanged();
  }

  public int getRootDaemonPort() {
    return getIntPref(
        R.string.pref_privs_root_daemon_port_enc_key,
        R.integer.pref_privs_root_daemon_port_enc_default);
  }

  public void saveRootDaemonPort(int port) {
    savePref(R.string.pref_privs_root_daemon_port_enc_key, port);
  }

  public int getAdbDaemonPort() {
    return getIntPref(
        R.string.pref_privs_adb_daemon_port_enc_key,
        R.integer.pref_privs_adb_daemon_port_enc_default);
  }

  public void saveAdbDaemonPort(int port) {
    savePref(R.string.pref_privs_adb_daemon_port_enc_key, port);
  }

  public int getAdbPort() {
    return getIntPref(R.string.pref_privs_adb_port_key, R.integer.pref_privs_adb_port_default);
  }

  public void saveAdbPort(int port) {
    if (port == 0) {
      getDefPrefs().edit().remove(getString(R.string.pref_privs_adb_port_key)).apply();
    } else {
      savePref(R.string.pref_privs_adb_port_key, port);
    }
  }

  private String mAdbHost = "127.0.0.1";

  public void setAdbHost(String host) {
    mAdbHost = host;
  }

  public String getAdbHost() {
    return mAdbHost;
  }

  public int getDaemonUid() {
    return Integer.parseInt(
        getStringPref(
            R.string.pref_adv_settings_daemon_uid_key,
            R.string.pref_adv_settings_daemon_uid_default));
  }

  public String getDaemonContext() {
    String cxt =
        getStringPref(
            R.string.pref_adv_settings_daemon_context_key,
            R.string.pref_adv_settings_daemon_context_default);
    return getString(R.string.pref_adv_settings_daemon_context_default).equals(cxt) ? null : cxt;
  }

  public boolean shouldDaemonExitOnAppDeath() {
    return getBoolPref(
        R.string.pref_adv_settings_exit_on_app_death_key,
        R.bool.pref_adv_settings_exit_on_app_death_default);
  }

  public int getDaemonPort() {
    return getIntPref(
        R.string.pref_privs_daemon_port_enc_key, R.integer.pref_privs_daemon_port_enc_default);
  }

  public void saveDaemonPort(int port) {
    savePref(R.string.pref_privs_daemon_port_enc_key, port);
  }

  public boolean shouldRestartDaemon() {
    return ApiUtils.getMyPkgInfo().lastUpdateTime
        > getLongPref(R.string.pref_privs_daemon_start_ts_enc_key);
  }

  public void setDaemonStartTs(long ts) {
    savePref(R.string.pref_privs_daemon_start_ts_enc_key, ts);
  }

  public String getSuExePath() {
    String path = getStringPref(R.string.pref_adv_settings_su_exe_path_key, 0);
    return TextUtils.isEmpty(path) ? "su" : path;
  }

  public boolean useUniqueRefForAppOpUidMode() {
    return getBoolPref(
        R.string.pref_adv_settings_unique_ref_app_op_uid_mode_key,
        R.bool.pref_adv_settings_unique_ref_app_op_uid_mode_default);
  }

  public boolean shouldFixPermDb() {
    return getBoolPref(R.string.pref_tmp_fix_perm_db_enc_key, true);
  }

  public void setFixPermDb(boolean fixPermDb) {
    savePref(R.string.pref_tmp_fix_perm_db_enc_key, fixPermDb);
  }

  private volatile String mQueryText;

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
    return getBoolPref(R.string.pref_main_deep_search_key, R.bool.pref_main_deep_search_default);
  }

  public void setDeepSearchEnabled(boolean enabled) {
    savePref(R.string.pref_main_deep_search_key, enabled);
  }

  public boolean isCaseSensitiveSearch() {
    return getBoolPref(
        R.string.pref_main_case_sensitive_search_key,
        R.bool.pref_main_case_sensitive_search_default);
  }

  public boolean isSpecialSearch() {
    return getBoolPref(
        R.string.pref_settings_special_search_key, R.bool.pref_settings_special_search_default);
  }

  public boolean getExcFiltersEnabled() {
    return getBoolPref(
        R.string.pref_filter_master_switch_key, R.bool.pref_filter_master_switch_default);
  }

  public void setExcFiltersEnabled(boolean enabled) {
    savePref(R.string.pref_filter_master_switch_key, enabled);
  }

  public void saveExcludedList(int key, Set<String> set) {
    getDefPrefs().edit().putStringSet(getString(key), set).apply();
    ExcFiltersData.INS.updateList(getString(key));
  }

  public boolean manuallyExcludeApps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_manually_exclude_apps_key,
            R.bool.pref_filter_manually_exclude_apps_default);
  }

  public boolean excludeNoIconApps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_no_icon_apps_key,
            R.bool.pref_filter_exclude_no_icon_apps_default);
  }

  public boolean excludeUserApps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_user_apps_key,
            R.bool.pref_filter_exclude_user_apps_default);
  }

  public boolean excludeSystemApps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_system_apps_key,
            R.bool.pref_filter_exclude_system_apps_default);
  }

  public boolean excludeFrameworkApps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_framework_apps_key,
            R.bool.pref_filter_exclude_framework_apps_default);
  }

  public boolean excludeDisabledApps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_disabled_apps_key,
            R.bool.pref_filter_exclude_disabled_apps_default);
  }

  public boolean excludeNoPermsApps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_no_perms_apps_key,
            R.bool.pref_filter_exclude_no_perms_apps_default);
  }

  public void clearExcludedAppsList() {
    saveExcludedList(R.string.pref_filter_excluded_apps_key, new HashSet<>());
  }

  public void addPkgToExcludedApps(String pkgName) {
    Set<String> excludedApps = getSetPref(R.string.pref_filter_excluded_apps_key);
    if (excludedApps == null) {
      excludedApps = new HashSet<>();
    } else {
      excludedApps = new HashSet<>(excludedApps);
    }
    excludedApps.add(pkgName);
    saveExcludedList(R.string.pref_filter_excluded_apps_key, excludedApps);
  }

  public boolean excludeInvalidPerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_invalid_perms_key,
            R.bool.pref_filter_exclude_invalid_perms_default);
  }

  public boolean excludeNotChangeablePerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_not_changeable_perms_key,
            R.bool.pref_filter_exclude_not_changeable_perms_default);
  }

  public boolean excludeNotGrantedPerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_not_granted_perms_key,
            R.bool.pref_filter_exclude_not_granted_perms_default);
  }

  public boolean manuallyExcludePerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_manually_exclude_perms_key,
            R.bool.pref_filter_manually_exclude_perms_default);
  }

  public boolean excludeNormalPerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_normal_perms_key,
            R.bool.pref_filter_exclude_normal_perms_default);
  }

  public boolean excludeDangerousPerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_dangerous_perms_key,
            R.bool.pref_filter_exclude_dangerous_perms_default);
  }

  public boolean excludeSignaturePerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_signature_perms_key,
            R.bool.pref_filter_exclude_signature_perms_default);
  }

  public boolean excludePrivilegedPerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_privileged_perms_key,
            R.bool.pref_filter_exclude_privileged_perms_default);
  }

  public void clearExcludedPermsList() {
    saveExcludedList(R.string.pref_filter_excluded_perms_key, new HashSet<>());
  }

  public void addPermToExcludedPerms(String permName) {
    Set<String> excludedPerms = getSetPref(R.string.pref_filter_excluded_perms_key);
    if (excludedPerms == null) {
      excludedPerms = new HashSet<>();
    } else {
      excludedPerms = new HashSet<>(excludedPerms);
    }
    excludedPerms.add(permName);
    saveExcludedList(R.string.pref_filter_excluded_perms_key, excludedPerms);
  }

  public boolean excludeAppOpsPerms() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_appops_perms_key,
            R.bool.pref_filter_exclude_appops_perms_default);
  }

  public boolean excludeNotSetAppOps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_exclude_not_set_appops_key,
            R.bool.pref_filter_exclude_not_set_appops_default);
  }

  public boolean showExtraAppOps() {
    return getExcFiltersEnabled()
        && getBoolPref(
            R.string.pref_filter_show_extra_app_ops_key,
            R.bool.pref_filter_show_extra_app_ops_default);
  }

  public void clearExtraAppOpsList() {
    saveExcludedList(R.string.pref_filter_extra_appops_key, new HashSet<>());
  }

  public static final int PREF_DRAWER_CHANGED = 0;
  public static final int PREF_UI_CHANGED = 1;

  public final LiveEvent<Integer> mPrefsWatcher = new LiveEvent<>(true);

  private void drawerPrefChanged() {
    mPrefsWatcher.postValue(PREF_DRAWER_CHANGED, true);
  }

  public void recreateMainActivity() {
    mPrefsWatcher.postValue(PREF_UI_CHANGED, true);
  }
}
