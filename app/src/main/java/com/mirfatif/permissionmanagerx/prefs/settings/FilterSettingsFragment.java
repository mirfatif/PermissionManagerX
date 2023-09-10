package com.mirfatif.permissionmanagerx.prefs.settings;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.graphics.Color;
import android.os.Bundle;
import android.text.SpannableString;
import android.text.Spanned;
import android.text.TextPaint;
import android.text.TextUtils;
import android.text.style.MetricAffectingSpan;
import androidx.core.util.Pair;
import androidx.preference.CheckBoxPreference;
import androidx.preference.MultiSelectListPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.fwk.FilterSettingsActivityM;
import com.mirfatif.permissionmanagerx.parser.AppOpsParser;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.parser.PermGroupsMapping;
import com.mirfatif.permissionmanagerx.parser.PkgParserFlavor;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.MySettingsFlavor;
import com.mirfatif.permissionmanagerx.prefs.fwk.CustomPrefDialogFrag;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.SmallDimMarginSpan;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import com.mirfatif.privtasks.Constants;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

public class FilterSettingsFragment extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private FilterSettingsActivityM mA;

  CheckBoxPreference excludeNoIconAppsView;
  CheckBoxPreference excludeUserAppsView;
  CheckBoxPreference excludeSystemAppsView;
  CheckBoxPreference excludeFrameworkAppsView;
  CheckBoxPreference excludeDisabledAppsView;
  CheckBoxPreference excludeNoPermsAppsView;
  CheckBoxPreference manuallyExcludeAppsView;
  private MultiSelectListPreference excludedAppsListView;

  CheckBoxPreference excludeNotChangeablePermsView;
  CheckBoxPreference excludeNotGrantedPermsView;
  CheckBoxPreference manuallyExcludePermsView;
  private MultiSelectListPreference excludedPermsListView;

  CheckBoxPreference excludeInvalidPermsView;
  CheckBoxPreference excludeNormalPermsView;
  CheckBoxPreference excludeDangerousPermsView;
  CheckBoxPreference excludeSignaturePermsView;
  CheckBoxPreference excludePrivilegedPermsView;

  CheckBoxPreference excludeAppOpsPermsView;
  CheckBoxPreference excludeNotSetAppOpsView;
  CheckBoxPreference showExtraAppOpsView;
  private MultiSelectListPreference extraAppOpsListView;

  public void onAttach(Context context) {
    super.onAttach(context);
    mA = (FilterSettingsActivityM) getActivity();
  }

  public void onPause() {
    MySettings.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
    super.onPause();
  }

  public void onResume() {
    super.onResume();
    MySettings.getDefPrefs().registerOnSharedPreferenceChangeListener(this);
  }

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
    FilterSettingsFragFlavor.onCreatePrefs(this);

    setHasOptionsMenu(true);
  }

  private void updateViews() {
    if (excludeUserAppsView == null) {
      return;
    }

    excludeNoIconAppsView.setChecked(MySettings.INS.excludeNoIconApps());
    excludeUserAppsView.setChecked(MySettings.INS.excludeUserApps());
    excludeSystemAppsView.setChecked(MySettings.INS.excludeSystemApps());
    excludeFrameworkAppsView.setChecked(MySettings.INS.excludeFrameworkApps());
    excludeDisabledAppsView.setChecked(MySettings.INS.excludeDisabledApps());
    excludeNoPermsAppsView.setChecked(MySettings.INS.excludeNoPermsApps());
    manuallyExcludeAppsView.setChecked(MySettings.INS.manuallyExcludeApps());

    excludeNotChangeablePermsView.setChecked(MySettings.INS.excludeNotChangeablePerms());
    excludeNotGrantedPermsView.setChecked(MySettings.INS.excludeNotGrantedPerms());
    manuallyExcludePermsView.setChecked(MySettings.INS.manuallyExcludePerms());

    excludeInvalidPermsView.setChecked(MySettings.INS.excludeInvalidPerms());
    excludeNormalPermsView.setChecked(MySettings.INS.excludeNormalPerms());
    excludeDangerousPermsView.setChecked(MySettings.INS.excludeDangerousPerms());
    excludeSignaturePermsView.setChecked(MySettings.INS.excludeSignaturePerms());
    excludePrivilegedPermsView.setChecked(MySettings.INS.excludePrivilegedPerms());

    excludeAppOpsPermsView.setChecked(MySettings.INS.excludeAppOpsPerms());
    excludeNotSetAppOpsView.setChecked(MySettings.INS.excludeNotSetAppOps());
    showExtraAppOpsView.setChecked(MySettings.INS.showExtraAppOps());

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

    updateExcludedAppsView(
        ExcFiltersData.INS.getExcludedApps(), ExcFiltersData.INS.getExcludedAppsLabels());

    Set<String> excPerms = ExcFiltersData.INS.getExcludedPerms();

    new LiveTasksQueueTyped<>(this, () -> buildExcPermNames(excPerms))
        .onUiWith(excPermsLabels -> updateExcludedPermsView(excPermsLabels, excPerms))
        .start();

    new LiveTasksQueueTyped<>(this, this::buildAppOpsNamesList)
        .onUiWith(
            appOps ->
                updateExtraAppOpsView(
                    appOps.first, appOps.second, ExcFiltersData.INS.getExtraAppOps()))
        .start();
  }

  private Pair<String[], CharSequence[]> buildAppOpsNamesList() {
    String[] appOps =
        AppOpsParser.INS.getAppOpsNames().stream()
            .sorted(Comparator.comparing(String::toUpperCase))
            .toArray(String[]::new);

    CharSequence[] labels = new CharSequence[appOps.length];
    String extraAppOp;
    SpannableString string;

    for (int i = 0; i < labels.length; i++) {
      extraAppOp = appOps[i];
      if (Constants.UNKNOWN_OP.equals(extraAppOp)) {
        string = new SpannableString(extraAppOp);
        string.setSpan(new RedTextSpan(), 0, string.length(), Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        labels[i] = string;
      } else {
        labels[i] = createPermName(extraAppOp, true);
      }
    }

    return new Pair<>(appOps, labels);
  }

  private static class RedTextSpan extends MetricAffectingSpan {

    public void updateDrawState(TextPaint tp) {
      tp.setColor(Color.RED);
    }

    public void updateMeasureState(TextPaint textPaint) {
      updateDrawState(textPaint);
    }
  }

  private CharSequence[] buildExcPermNames(Set<String> excludedPerms) {
    List<String> appOpsNames = AppOpsParser.INS.getAppOpsNames();
    return excludedPerms.stream()
        .map(
            perm ->
                createPermName(
                    perm, appOpsNames.isEmpty() ? !perm.contains(".") : appOpsNames.contains(perm)))
        .toArray(CharSequence[]::new);
  }

  private static CharSequence createPermName(String permName, boolean isAppOp) {
    if (!MySettingsFlavor.INS.showFrameworkPermNames()) {
      CharSequence name =
          PkgParserFlavor.INS.getPermName(
              permName, PermGroupsMapping.INS.getGroupId(permName, isAppOp));
      if (!name.toString().equals(permName)) {
        SpannableString ss = new SpannableString(permName);
        ss.setSpan(
            new SmallDimMarginSpan(), 0, permName.length(), Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        return TextUtils.concat(name, "\n", ss);
      }
    }

    return permName;
  }

  private void updateExcludedAppsView(
      Set<String> excludedAppsSet, CharSequence[] excludedAppsLabels) {
    CharSequence[] excludedApps = excludedAppsSet.toArray(new String[0]);
    int appCount = excludedAppsSet.size();

    excludedAppsListView.setEntries(excludedAppsLabels);
    excludedAppsListView.setEntryValues(excludedApps);
    excludedAppsListView.setValues(excludedAppsSet);

    if (appCount == 0) {
      excludedAppsListView.setSummary(R.string.excluded_apps_summary);
    } else {
      String message = excludedApps[0].toString();
      int count = appCount - 1;
      if (count > 0) {
        message = ApiUtils.getQtyString(R.plurals.and_others_count, count, message, count);
      }
      excludedAppsListView.setSummary(message);
    }
    excludedAppsListView.setEnabled(manuallyExcludeAppsView.isChecked() && appCount != 0);
  }

  private void updateExcludedPermsView(
      CharSequence[] excludedPermsLabels, Set<String> excludedPermsSet) {
    CharSequence[] excludedPerms = excludedPermsSet.toArray(new String[0]);
    int permCount = excludedPermsSet.size();

    excludedPermsListView.setEntries(excludedPermsLabels);
    excludedPermsListView.setEntryValues(excludedPerms);
    excludedPermsListView.setValues(excludedPermsSet);

    if (permCount == 0) {
      excludedPermsListView.setSummary(R.string.excluded_perms_summary);
    } else {
      String message = excludedPerms[0].toString();
      int count = permCount - 1;
      if (count > 0) {
        message = ApiUtils.getQtyString(R.plurals.and_others_count, count, message, count);
      }
      excludedPermsListView.setSummary(message);
    }
    excludedPermsListView.setEnabled(manuallyExcludePermsView.isChecked() && permCount != 0);
  }

  private void updateExtraAppOpsView(
      String[] appOps, CharSequence[] appOpsLabels, Set<String> extraAppOps) {

    int appOpsCount = appOpsLabels.length;

    if (appOpsCount != 0) {

      extraAppOpsListView.setEntries(appOpsLabels);

      extraAppOpsListView.setEntryValues(appOps);

      extraAppOpsListView.setValues(extraAppOps);
    }

    int extraAppOpsCount = extraAppOps.size();

    String message;
    if (extraAppOpsCount == 0 || appOpsCount == 0) {
      message = getString(R.string.extra_app_ops_summary);
      if (appOpsCount != 0) {
        message += " " + getString(R.string.extra_app_ops_summary_count, appOpsCount);
      }
    } else {
      message = (String) extraAppOps.toArray()[0];
      extraAppOpsCount--;

      if (extraAppOpsCount > 0) {
        message =
            ApiUtils.getQtyString(
                R.plurals.and_others_count, extraAppOpsCount, message, extraAppOpsCount);
      }
    }
    extraAppOpsListView.setSummary(message);
    extraAppOpsListView.setEnabled(showExtraAppOpsView.isChecked() && appOpsCount != 0);
  }

  public void onDisplayPreferenceDialog(Preference preference) {
    if (!CustomPrefDialogFrag.showPrefDialogFrag(preference, this)) {
      super.onDisplayPreferenceDialog(preference);
    }
  }

  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {

    ExcFiltersData.INS.updateList(key);

    updateViews();

    mA.invalidateOptionsMenu();

    PackageParser.INS.updatePkgList();
  }
}
