package com.mirfatif.permissionmanagerx.prefs;

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
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

// OnSharedPreferenceChangeListener must be global to avoid GC
public class FilterSettingsFragment extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private FilterSettingsActivity mParentActivity;
  private MySettings mMySettings;

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
    Utils.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
    mMySettings = MySettings.getInstance();
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
    excludedAppsView = findPreference(getString(R.string.pref_filter_excluded_apps_key));

    excludeInvalidPermsView =
        findPreference(getString(R.string.pref_filter_exclude_invalid_perms_key));
    excludeNotChangeablePermsView =
        findPreference(getString(R.string.pref_filter_exclude_not_changeable_perms_key));
    excludeNotGrantedPermsView =
        findPreference(getString(R.string.pref_filter_exclude_not_granted_perms_key));
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
    excludedPermsView = findPreference(getString(R.string.pref_filter_excluded_perms_key));
    extraAppOpsView = findPreference(getString(R.string.pref_filter_extra_appops_key));

    updateViews();

    // so that invalidateOptionsMenu() can be called later
    setHasOptionsMenu(true);
  }

  private void updateViews() {
    if (excludeUserAppsView == null) {
      return;
    }

    // required on Reset to Defaults
    excludeNoIconAppsView.setChecked(mMySettings.excludeNoIconApps());
    excludeUserAppsView.setChecked(mMySettings.excludeUserApps());
    excludeSystemAppsView.setChecked(mMySettings.excludeSystemApps());
    excludeFrameworkAppsView.setChecked(mMySettings.excludeFrameworkApps());
    excludeDisabledAppsView.setChecked(mMySettings.excludeDisabledApps());
    excludeNoPermsAppsView.setChecked(mMySettings.excludeNoPermissionsApps());
    excludeNoPermsAppsView.setVisible(!mMySettings.isQuickScan());
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

    if (excludeAppOpsPermsView.isChecked()) {
      if (!excludeNotSetAppOpsView.isChecked()) {
        excludeNotSetAppOpsView.setChecked(true);
      }
      excludeNotSetAppOpsView.setEnabled(false);
      extraAppOpsView.setEnabled(false);
    } else {
      excludeNotSetAppOpsView.setEnabled(true);
      extraAppOpsView.setEnabled(true);
    }

    // Lists in Preferences must have been updated before starting settings fragment, otherwise
    // MultiSelectPreferenceList shows unchecked items.
    // Or handle them manually as below.

    // Apps
    Utils.runInBg(
        () -> {
          mMySettings.getExcludedAppsLock();
          Set<String> excludedApps = mMySettings.getExcludedApps();
          Utils.runInFg(() -> updateExcludedAppsView(excludedApps));
          mMySettings.releaseExcludedAppsLock();
        });

    // Permissions
    Utils.runInBg(
        () -> {
          mMySettings.getExcludedPermsLock();
          Set<String> excludedPerms = mMySettings.getExcludedPerms();
          Utils.runInFg(() -> updateExcludedPermsView(excludedPerms));
          mMySettings.releaseExcludedPermsLock();
        });

    // Extra AppOps
    Utils.runInBg(
        () -> {
          mMySettings.getExtraAppOpsLock();
          // Need to be on background thread.
          List<String> appOpsList = new ArrayList<>(mMySettings.getAppOpsList());
          appOpsList.sort(Comparator.comparing(String::toUpperCase));
          Set<String> extraAppOps = mMySettings.getExtraAppOps();

          Utils.runInFg(
              () -> updateExtraAppOpsView(appOpsList.toArray(new String[0]), extraAppOps));
          mMySettings.releaseExtraAppOpsLock();
        });
  }

  private void updateExcludedAppsView(Set<String> excludedAppsSet) {
    CharSequence[] excludedApps = excludedAppsSet.toArray(new String[0]);
    int appCount = mMySettings.getExcludedAppsCount();

    excludedAppsView.setEntries(mMySettings.getExcludedAppsLabels());
    excludedAppsView.setEntryValues(excludedApps);
    excludedAppsView.setValues(excludedAppsSet);

    // Disable if no excluded apps
    if (appCount == 0) {
      excludedAppsView.setEnabled(false);
      excludedAppsView.setSummary(R.string.excluded_apps_summary);
    } else {
      excludedAppsView.setEnabled(true);
      String message = excludedApps[0].toString();
      if (appCount == 2) {
        message += " " + getString(R.string.and_other_count, 1);
      } else if (appCount > 2) {
        message += " " + getString(R.string.and_others_count, appCount - 1);
      }
      excludedAppsView.setSummary(message);
    }
  }

  private void updateExcludedPermsView(Set<String> excludedPermsSet) {
    CharSequence[] excludedPerms = excludedPermsSet.toArray(new String[0]);
    int permCount = mMySettings.getExcludedPermsCount();

    excludedPermsView.setEntries(excludedPerms);
    excludedPermsView.setEntryValues(excludedPerms);
    excludedPermsView.setValues(excludedPermsSet);

    // Disable if no excluded permissions
    if (permCount == 0) {
      excludedPermsView.setEnabled(false);
      excludedPermsView.setSummary(R.string.excluded_perms_summary);
    } else {
      excludedPermsView.setEnabled(true);
      String message = excludedPerms[0].toString();
      if (permCount == 2) {
        message += " " + getString(R.string.and_other_count, 1);
      } else if (permCount > 2) {
        message += " " + getString(R.string.and_others_count, permCount - 1);
      }
      excludedPermsView.setSummary(message);
    }
  }

  private void updateExtraAppOpsView(CharSequence[] appOpsList, Set<String> extraAppOps) {
    // Handle MySettings.excludeAppOpsPerms() case.
    if (appOpsList.length == 0) {
      extraAppOpsView.setEnabled(false);
    } else {
      // Item names in list
      extraAppOpsView.setEntries(appOpsList);
      // Corresponding values of shown items. EntryValues must exactly correspond to Entries.
      extraAppOpsView.setEntryValues(appOpsList);

      /** Do not set this accidentally if {@link MySettings#getExtraAppOps()} returns empty list */
      extraAppOpsView.setValues(extraAppOps); // Checked entry values
    }

    int count = mMySettings.getExtraAppOpsCount();

    // Set summary
    if (count == 0 || !extraAppOpsView.isEnabled()) {
      String message = getString(R.string.extra_app_ops_summary);
      if (extraAppOpsView.isEnabled()) {
        message += " " + getString(R.string.extra_app_ops_summary_count, appOpsList.length);
      }
      extraAppOpsView.setSummary(message);
    } else {
      String message = (String) extraAppOps.toArray()[0];
      if (count == 2) {
        // Without providing context, getString() crashes with: "Fragment ... not attached to a
        // context" on rotation. getActivity() may also return null.
        message += " " + mParentActivity.getString(R.string.and_other_count, 1);
      } else if (count > 2) {
        message += " " + getString(R.string.and_others_count, count - 1);
      }
      extraAppOpsView.setSummary(message);
    }
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
    mMySettings.updateList(key);

    updateViews();

    // force recreate Options Menu to reset "Clear ..." items
    mParentActivity.invalidateOptionsMenu();

    // update packages list when a Preference is changed so that RecyclerView is updated on
    // return to MainActivity
    PackageParser.getInstance().updatePackagesList();
  }
}
