package com.mirfatif.permissionmanagerx;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.preference.CheckBoxPreference;
import androidx.preference.MultiSelectListPreference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.PreferenceManager;
import java.util.Set;

// OnSharedPreferenceChangeListener must be global to avoid GC
public class FilterSettingsFragment extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  FilterSettingsActivity mParentActivity;
  MySettings mMySettings;

  private CheckBoxPreference excludeNoIconAppsView;
  private CheckBoxPreference excludeUserAppsView;
  private CheckBoxPreference excludeSystemAppsView;
  private CheckBoxPreference excludeFrameworkAppsView;
  private CheckBoxPreference excludeDisabledAppsView;
  private CheckBoxPreference excludeNoPermsAppsView;
  private MultiSelectListPreference excludedAppsView;

  private CheckBoxPreference excludeInvalidPermsView;
  private CheckBoxPreference excludeNotChangeablePermsView;
  private CheckBoxPreference excludeNotGrantedPermsView;
  private CheckBoxPreference excludeNormalPermsView;
  private CheckBoxPreference excludeDangerousPermsView;
  private CheckBoxPreference excludeSignaturePermsView;
  private CheckBoxPreference excludePrivilegedPermsView;
  private CheckBoxPreference excludeAppOpsPermsView;
  private CheckBoxPreference excludeNotSetAppOpsView;
  private MultiSelectListPreference excludedPermsView;
  private MultiSelectListPreference extraAppOpsView;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mParentActivity = (FilterSettingsActivity) getActivity();
    PreferenceManager.getDefaultSharedPreferences(context)
        .unregisterOnSharedPreferenceChangeListener(this);
    mMySettings = MySettings.getInstance();
  }

  // unregister so that changes in other activities don't trigger this
  @Override
  public void onPause() {
    super.onPause();
    PreferenceManager.getDefaultSharedPreferences(mParentActivity)
        .unregisterOnSharedPreferenceChangeListener(this);
  }

  @Override
  public void onResume() {
    super.onResume();
    PreferenceManager.getDefaultSharedPreferences(mParentActivity)
        .registerOnSharedPreferenceChangeListener(this);
  }

  @Override
  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.filter_settings_preferences, rootKey);

    excludeNoIconAppsView =
        findPreference(getString(R.string.filter_settings_exclude_no_icon_apps_key));
    excludeUserAppsView = findPreference(getString(R.string.filter_settings_exclude_user_apps_key));
    excludeSystemAppsView =
        findPreference(getString(R.string.filter_settings_exclude_system_apps_key));
    excludeFrameworkAppsView =
        findPreference(getString(R.string.filter_settings_exclude_framework_apps_key));
    excludeDisabledAppsView =
        findPreference(getString(R.string.filter_settings_exclude_disabled_apps_key));
    excludeNoPermsAppsView =
        findPreference(getString(R.string.filter_settings_exclude_no_perms_apps_key));
    excludedAppsView = findPreference(getString(R.string.filter_settings_excluded_apps_key));

    excludeInvalidPermsView =
        findPreference(getString(R.string.filter_settings_exclude_invalid_perms_key));
    excludeNotChangeablePermsView =
        findPreference(getString(R.string.filter_settings_exclude_not_changeable_perms_key));
    excludeNotGrantedPermsView =
        findPreference(getString(R.string.filter_settings_exclude_not_granted_perms_key));
    excludeNormalPermsView =
        findPreference(getString(R.string.filter_settings_exclude_normal_perms_key));
    excludeDangerousPermsView =
        findPreference(getString(R.string.filter_settings_exclude_dangerous_perms_key));
    excludeSignaturePermsView =
        findPreference(getString(R.string.filter_settings_exclude_signature_perms_key));
    excludePrivilegedPermsView =
        findPreference(getString(R.string.filter_settings_exclude_privileged_perms_key));
    excludeAppOpsPermsView =
        findPreference(getString(R.string.filter_settings_exclude_appops_perms_key));
    excludeNotSetAppOpsView =
        findPreference(getString(R.string.filter_settings_exclude_not_set_appops_key));
    excludedPermsView = findPreference(getString(R.string.filter_settings_excluded_perms_key));
    extraAppOpsView = findPreference(getString(R.string.filter_settings_extra_appops_key));

    updateViews();

    // so that invalidateOptionsMenu() can be called later
    setHasOptionsMenu(true);
  }

  private void updateViews() {
    if (excludeUserAppsView == null) return;

    // required on Reset to Defaults
    excludeNoIconAppsView.setChecked(mMySettings.excludeNoIconApps());
    excludeUserAppsView.setChecked(mMySettings.excludeUserApps());
    excludeSystemAppsView.setChecked(mMySettings.excludeSystemApps());
    excludeFrameworkAppsView.setChecked(mMySettings.excludeFrameworkApps());
    excludeDisabledAppsView.setChecked(mMySettings.excludeDisabledApps());
    excludeNoPermsAppsView.setChecked(mMySettings.excludeNoPermissionsApps());
    excludeInvalidPermsView.setChecked(mMySettings.excludeInvalidPermissions());
    excludeNotChangeablePermsView.setChecked(mMySettings.excludeNotChangeablePerms());
    excludeNotGrantedPermsView.setChecked(mMySettings.excludeNotGrantedPerms());
    excludeNormalPermsView.setChecked(mMySettings.excludeNormalPerms());
    excludeDangerousPermsView.setChecked(mMySettings.excludeDangerousPerms());
    excludeSignaturePermsView.setChecked(mMySettings.excludeSignaturePerms());
    excludePrivilegedPermsView.setChecked(mMySettings.excludePrivilegedPerms());
    excludeAppOpsPermsView.setChecked(mMySettings.excludeAppOpsPerms());
    excludeNotSetAppOpsView.setChecked(mMySettings.excludeNotSetAppOps());

    // required on manually changed by tapping
    if (excludeUserAppsView.isChecked()) {
      if (excludeSystemAppsView.isChecked()) excludeSystemAppsView.setChecked(false);
      excludeSystemAppsView.setEnabled(false);
    } else {
      excludeSystemAppsView.setEnabled(true);
    }

    if (excludeSystemAppsView.isChecked()) {
      if (!excludeFrameworkAppsView.isChecked()) excludeFrameworkAppsView.setChecked(true);
      excludeFrameworkAppsView.setEnabled(false);
      if (excludeUserAppsView.isChecked()) excludeUserAppsView.setChecked(false);
      excludeUserAppsView.setEnabled(false);
    } else {
      excludeFrameworkAppsView.setEnabled(true);
      excludeUserAppsView.setEnabled(true);
    }

    if (excludeAppOpsPermsView.isChecked()) {
      if (!excludeNotSetAppOpsView.isChecked()) excludeNotSetAppOpsView.setChecked(true);
      excludeNotSetAppOpsView.setEnabled(false);
      extraAppOpsView.setEnabled(false);
    } else {
      excludeNotSetAppOpsView.setEnabled(true);
      extraAppOpsView.setEnabled(true);
    }

    // Lists in Preferences must have been updated before starting settings fragment, otherwise
    // MultiSelectPreferenceList shows unchecked items.
    // Or handle them manually as below

    // apps
    CharSequence[] entries = mMySettings.getExcludedAppsLabels();
    CharSequence[] entryValues = mMySettings.getExcludedAppsNames();
    int count = mMySettings.getExcludedAppsCount();

    excludedAppsView.setEntries(entries); // item names in list
    excludedAppsView.setEntryValues(entryValues); // corresponding values of shown items
    excludedAppsView.setValues(mMySettings.getExcludedAppsSet()); // checked entry values

    // disable if no excluded apps
    if (count == 0) {
      excludedAppsView.setEnabled(false);
      excludedAppsView.setSummary(R.string.excluded_apps_summary);
    } else {
      excludedAppsView.setEnabled(true);
      CharSequence message = entryValues[0];
      if (count == 2) {
        message += " " + getString(R.string.and_other_count, 1);
      } else if (count > 2) {
        message += " " + getString(R.string.and_others_count, count - 1);
      }
      excludedAppsView.setSummary(message);
    }

    // permissions
    entries = mMySettings.getExcludedPermsNames();
    count = mMySettings.getExcludedPermsCount();
    excludedPermsView.setEntries(entries);
    excludedPermsView.setEntryValues(entries);

    // disable if no excluded permissions
    if (count == 0) {
      excludedPermsView.setEnabled(false);
      excludedPermsView.setSummary(R.string.excluded_perms_summary);
    } else {
      excludedPermsView.setEnabled(true);
      CharSequence message = entries[0];
      if (count == 2) {
        message += " " + getString(R.string.and_other_count, 1);
      } else if (count > 2) {
        message += " " + getString(R.string.and_others_count, count - 1);
      }
      excludedPermsView.setSummary(message);
    }

    // extra AppOps
    Utils.runInBg(
        () -> {
          Set<String> values = mMySettings.getExtraAppOps();

          // needs to be on background thread
          CharSequence[] _entries = mMySettings.getAppOpsListSorted();

          Utils.runInFg(() -> updateExtraAppOpsView(_entries, values));
        });
  }

  private void updateExtraAppOpsView(CharSequence[] entries, Set<String> values) {
    // handle MySettings.excludeAppOpsPerms() case
    if (entries.length == 0) {
      extraAppOpsView.setEnabled(false);
    } else {
      extraAppOpsView.setEntries(entries); // item names in list
      extraAppOpsView.setEntryValues(entries); // corresponding values of shown items

      /** do not set this accidentally if {@link MySettings#getExtraAppOps()} returns empty list */
      extraAppOpsView.setValues(values); // checked entry values
    }

    int count = mMySettings.getExtraAppOpsCount();

    // set summary
    if (count == 0 || !extraAppOpsView.isEnabled()) {
      String message = getString(R.string.extra_app_ops_summary);
      if (extraAppOpsView.isEnabled()) {
        message += " " + getString(R.string.extra_app_ops_summary_count, entries.length);
      }
      extraAppOpsView.setSummary(message);
    } else {
      String message = (String) values.toArray()[0];
      if (count == 2) {
        message += " " + getString(R.string.and_other_count, 1);
      } else if (count > 2) {
        message += " " + getString(R.string.and_others_count, count - 1);
      }
      extraAppOpsView.setSummary(message);
    }
  }

  @Override
  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    // only lists need to be updated if changed from Preferences screen
    mMySettings.updateList(key);

    updateViews();

    // force recreate Options Menu to reset "Clear ..." items
    mParentActivity.invalidateOptionsMenu();

    // update packages list when a Preference is changed so that RecyclerView is updated on
    // return to MainActivity
    PackageParser.getInstance().updatePackagesList(true);
  }
}
