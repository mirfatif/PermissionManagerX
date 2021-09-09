package com.mirfatif.permissionmanagerx.prefs;

import static com.mirfatif.permissionmanagerx.parser.AppOpsParser.APP_OPS_PARSER;
import static com.mirfatif.permissionmanagerx.parser.PackageParser.PKG_PARSER;
import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.preference.CheckBoxPreference;
import androidx.preference.MultiSelectListPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

// OnSharedPreferenceChangeListener must be global to avoid GC
public class FilterSettingsFragment extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private FilterSettingsActivity mParentActivity;

  private CheckBoxPreference excludeNoIconAppsView;
  private CheckBoxPreference excludeUserAppsView;
  private CheckBoxPreference excludeSystemAppsView;
  private CheckBoxPreference excludeFrameworkAppsView;
  private CheckBoxPreference excludeDisabledAppsView;
  private CheckBoxPreference excludeNoPermsAppsView;
  private CheckBoxPreference manuallyExcludeAppsView;
  private MultiSelectListPreference excludedAppsListView;

  private CheckBoxPreference excludeNotChangeablePermsView;
  private CheckBoxPreference excludeNotGrantedPermsView;
  private CheckBoxPreference manuallyExcludePermsView;
  private MultiSelectListPreference excludedPermsListView;

  private CheckBoxPreference excludeInvalidPermsView;
  private CheckBoxPreference excludeNormalPermsView;
  private CheckBoxPreference excludeDangerousPermsView;
  private CheckBoxPreference excludeSignaturePermsView;
  private CheckBoxPreference excludePrivilegedPermsView;

  private CheckBoxPreference excludeAppOpsPermsView;
  private CheckBoxPreference excludeNotSetAppOpsView;
  private CheckBoxPreference showExtraAppOpsView;
  private MultiSelectListPreference extraAppOpsListView;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mParentActivity = (FilterSettingsActivity) getActivity();
    Utils.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
  }

  // unregister so that changes in other activities don't trigger this
  @Override
  public void onPause() {
    Utils.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
    super.onPause();
  }

  @Override
  public void onResume() {
    super.onResume();
    Utils.getDefPrefs().registerOnSharedPreferenceChangeListener(this);
  }

  @Override
  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.filter_settings_preferences, rootKey);

    excludeNoIconAppsView =
        findPreference(getString(R.string.pref_filter_exclude_no_icon_apps_key));
    excludeUserAppsView = findPreference(getString(R.string.pref_filter_exclude_user_apps_key));
    excludeSystemAppsView = findPreference(getString(R.string.pref_filter_exclude_system_apps_key));
    excludeFrameworkAppsView =
        findPreference(getString(R.string.pref_filter_exclude_framework_apps_key));
    excludeDisabledAppsView =
        findPreference(getString(R.string.pref_filter_exclude_disabled_apps_key));
    excludeNoPermsAppsView =
        findPreference(getString(R.string.pref_filter_exclude_no_perms_apps_key));
    manuallyExcludeAppsView =
        findPreference(getString(R.string.pref_filter_manually_exclude_apps_key));
    excludedAppsListView = findPreference(getString(R.string.pref_filter_excluded_apps_key));

    excludeNotChangeablePermsView =
        findPreference(getString(R.string.pref_filter_exclude_not_changeable_perms_key));
    excludeNotGrantedPermsView =
        findPreference(getString(R.string.pref_filter_exclude_not_granted_perms_key));
    manuallyExcludePermsView =
        findPreference(getString(R.string.pref_filter_manually_exclude_perms_key));
    excludedPermsListView = findPreference(getString(R.string.pref_filter_excluded_perms_key));

    excludeInvalidPermsView =
        findPreference(getString(R.string.pref_filter_exclude_invalid_perms_key));
    excludeNormalPermsView =
        findPreference(getString(R.string.pref_filter_exclude_normal_perms_key));
    excludeDangerousPermsView =
        findPreference(getString(R.string.pref_filter_exclude_dangerous_perms_key));
    excludeSignaturePermsView =
        findPreference(getString(R.string.pref_filter_exclude_signature_perms_key));
    excludePrivilegedPermsView =
        findPreference(getString(R.string.pref_filter_exclude_privileged_perms_key));

    excludeAppOpsPermsView =
        findPreference(getString(R.string.pref_filter_exclude_appops_perms_key));
    excludeNotSetAppOpsView =
        findPreference(getString(R.string.pref_filter_exclude_not_set_appops_key));
    showExtraAppOpsView = findPreference(getString(R.string.pref_filter_show_extra_app_ops_key));
    extraAppOpsListView = findPreference(getString(R.string.pref_filter_extra_appops_key));

    updateViews();

    // so that invalidateOptionsMenu() can be called later
    setHasOptionsMenu(true);
  }

  private void updateViews() {
    if (excludeUserAppsView == null) {
      return;
    }

    // required on Reset to Defaults
    excludeNoIconAppsView.setChecked(SETTINGS.excludeNoIconApps());
    excludeUserAppsView.setChecked(SETTINGS.excludeUserApps());
    excludeSystemAppsView.setChecked(SETTINGS.excludeSystemApps());
    excludeFrameworkAppsView.setChecked(SETTINGS.excludeFrameworkApps());
    excludeDisabledAppsView.setChecked(SETTINGS.excludeDisabledApps());
    excludeNoPermsAppsView.setChecked(SETTINGS.excludeNoPermissionsApps());
    excludeNoPermsAppsView.setVisible(!SETTINGS.isQuickScanEnabled());
    manuallyExcludeAppsView.setChecked(SETTINGS.manuallyExcludeApps());

    excludeNotChangeablePermsView.setChecked(SETTINGS.excludeNotChangeablePerms());
    excludeNotGrantedPermsView.setChecked(SETTINGS.excludeNotGrantedPerms());
    manuallyExcludePermsView.setChecked(SETTINGS.manuallyExcludePerms());

    excludeInvalidPermsView.setChecked(SETTINGS.excludeInvalidPermissions());
    excludeNormalPermsView.setChecked(SETTINGS.excludeNormalPerms());
    excludeDangerousPermsView.setChecked(SETTINGS.excludeDangerousPerms());
    excludeSignaturePermsView.setChecked(SETTINGS.excludeSignaturePerms());
    excludePrivilegedPermsView.setChecked(SETTINGS.excludePrivilegedPerms());

    excludeAppOpsPermsView.setChecked(SETTINGS.excludeAppOpsPerms());
    excludeNotSetAppOpsView.setChecked(SETTINGS.excludeNotSetAppOps());
    showExtraAppOpsView.setChecked(SETTINGS.showExtraAppOps());

    // required on manually changed by tapping
    if (excludeUserAppsView.isChecked()) {
      if (excludeSystemAppsView.isChecked()) {
        excludeSystemAppsView.setChecked(false);
      }
      excludeSystemAppsView.setEnabled(false);
    } else {
      excludeSystemAppsView.setEnabled(true);
    }

    if (excludeSystemAppsView.isChecked()) {
      if (!excludeFrameworkAppsView.isChecked()) {
        excludeFrameworkAppsView.setChecked(true);
      }
      excludeFrameworkAppsView.setEnabled(false);
      if (excludeUserAppsView.isChecked()) {
        excludeUserAppsView.setChecked(false);
      }
      excludeUserAppsView.setEnabled(false);
    } else {
      excludeFrameworkAppsView.setEnabled(true);
      excludeUserAppsView.setEnabled(true);
    }

    boolean showAppOps = !excludeAppOpsPermsView.isChecked();
    excludeNotSetAppOpsView.setVisible(showAppOps);
    showExtraAppOpsView.setVisible(showAppOps);
    extraAppOpsListView.setVisible(showAppOps);

    // Lists in Preferences must have been updated before starting settings fragment, otherwise
    // MultiSelectPreferenceList shows unchecked items.
    // Or handle them manually as below.

    // Apps
    Utils.runInBg(
        () -> {
          SETTINGS.getExcludedAppsLock();
          Set<String> excludedApps = SETTINGS.getExcludedApps();
          CharSequence[] excludedAppsLabels = SETTINGS.getExcludedAppsLabels();
          Utils.runInFg(this, () -> updateExcludedAppsView(excludedApps, excludedAppsLabels));
          SETTINGS.releaseExcludedAppsLock();
        });

    // Permissions
    Utils.runInBg(
        () -> {
          SETTINGS.getExcludedPermsLock();
          Set<String> excludedPerms = SETTINGS.getExcludedPerms();
          Utils.runInFg(this, () -> updateExcludedPermsView(excludedPerms));
          SETTINGS.releaseExcludedPermsLock();
        });

    // Extra AppOps
    Utils.runInBg(
        () -> {
          SETTINGS.getExtraAppOpsLock();
          List<String> appOpsList = new ArrayList<>(APP_OPS_PARSER.getAppOpsList());
          appOpsList.sort(Comparator.comparing(String::toUpperCase));
          Set<String> extraAppOps = SETTINGS.getExtraAppOps();

          Utils.runInFg(
              this, () -> updateExtraAppOpsView(appOpsList.toArray(new String[0]), extraAppOps));
          SETTINGS.releaseExtraAppOpsLock();
        });
  }

  private void updateExcludedAppsView(
      Set<String> excludedAppsSet, CharSequence[] excludedAppsLabels) {
    CharSequence[] excludedApps = excludedAppsSet.toArray(new String[0]);
    int appCount = excludedAppsSet.size();

    excludedAppsListView.setEntries(excludedAppsLabels);
    excludedAppsListView.setEntryValues(excludedApps);
    excludedAppsListView.setValues(excludedAppsSet);

    // Disable if no excluded apps
    if (appCount == 0) {
      excludedAppsListView.setSummary(R.string.excluded_apps_summary);
    } else {
      String message = excludedApps[0].toString();
      int count = appCount - 1;
      message = Utils.getQtyString(R.plurals.and_others_count, count, message, count);
      excludedAppsListView.setSummary(message);
    }
    excludedAppsListView.setEnabled(manuallyExcludeAppsView.isChecked() && appCount != 0);
  }

  private void updateExcludedPermsView(Set<String> excludedPermsSet) {
    CharSequence[] excludedPerms = excludedPermsSet.toArray(new String[0]);
    int permCount = excludedPermsSet.size();

    excludedPermsListView.setEntries(excludedPerms);
    excludedPermsListView.setEntryValues(excludedPerms);
    excludedPermsListView.setValues(excludedPermsSet);

    // Disable if no excluded permissions
    if (permCount == 0) {
      excludedPermsListView.setSummary(R.string.excluded_perms_summary);
    } else {
      String message = excludedPerms[0].toString();
      int count = permCount - 1;
      message = Utils.getQtyString(R.plurals.and_others_count, count, message, count);
      excludedPermsListView.setSummary(message);
    }
    excludedPermsListView.setEnabled(manuallyExcludePermsView.isChecked() && permCount != 0);
  }

  private void updateExtraAppOpsView(CharSequence[] appOpsList, Set<String> extraAppOps) {
    // Returned AppOps list is empty if unable to read AppOps due to no privileges.
    int appOpsCount = appOpsList.length;

    if (appOpsCount != 0) {
      // Item names in list
      extraAppOpsListView.setEntries(appOpsList);
      // Corresponding values of shown items. EntryValues must exactly correspond to Entries.
      extraAppOpsListView.setEntryValues(appOpsList);
      // Checked entry values. This overwrites previously saved values. So the returned list
      // must not be empty (due to missing privileges).
      extraAppOpsListView.setValues(extraAppOps);
    }

    int extraAppOpsCount = extraAppOps.size();

    // Set summary
    if (extraAppOpsCount == 0 || appOpsCount == 0) {
      String message = getString(R.string.extra_app_ops_summary);
      if (appOpsCount != 0) {
        message += " " + getString(R.string.extra_app_ops_summary_count, appOpsCount);
      }
      extraAppOpsListView.setSummary(message);
    } else {
      String message = (String) extraAppOps.toArray()[0];
      extraAppOpsCount--;
      // Without providing context, getString() crashes with: "Fragment ... not attached to a
      // context" on rotation. getActivity() may also return null.
      extraAppOpsListView.setSummary(
          Utils.getQtyString(
              R.plurals.and_others_count, extraAppOpsCount, message, extraAppOpsCount));
    }
    extraAppOpsListView.setEnabled(showExtraAppOpsView.isChecked() && appOpsCount != 0);
  }

  @Override
  public void onDisplayPreferenceDialog(Preference preference) {
    // We need to override MultiSelectListPreference with a custom-built
    // DialogFragment in order to customize AlertDialog.
    if (preference instanceof MultiSelectListPreference) {
      final String TAG_MULTI_SELECT_LIST_DIALOG = "MULTI_SELECT_LIST_DIALOG";

      if (getParentFragmentManager().findFragmentByTag(TAG_MULTI_SELECT_LIST_DIALOG) == null) {
        MultiSelectListPrefDialogFrag fragment =
            MultiSelectListPrefDialogFrag.newInstance(preference.getKey());
        setTargetFragment(fragment);
        fragment.show(getParentFragmentManager(), TAG_MULTI_SELECT_LIST_DIALOG);
      }
      return;
    }
    super.onDisplayPreferenceDialog(preference);
  }

  // Without this onCreate() crashes because getTargetFragment() returns null
  @SuppressWarnings("deprecation")
  private void setTargetFragment(Fragment fragment) {
    fragment.setTargetFragment(this, 0);
  }

  @Override
  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    // only lists need to be updated if changed from Preferences screen
    SETTINGS.updateList(key);

    updateViews();

    // force recreate Options Menu to reset "Clear ..." items
    mParentActivity.invalidateOptionsMenu();

    // update packages list when a Preference is changed so that RecyclerView is updated on
    // return to MainActivity
    PKG_PARSER.updatePackagesList();
  }
}
