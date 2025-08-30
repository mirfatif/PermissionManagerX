package com.mirfatif.permissionmanagerx.prefs.settings;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.fwk.MainActivityM;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import java.util.Objects;

public class SearchSettingsFrag extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private MainActivityM mA;

  public void onAttach(Context context) {
    super.onAttach(context);
    mA = (MainActivityM) getActivity();
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
    setPreferencesFromResource(R.xml.settings_prefs_search, rootKey);
    Preference pref =
        findPreference(getString(R.string.pref_settings_search_suggestions_count_key));
    Objects.requireNonNull(pref)
        .setTitle(getString(R.string.pref_settings_search_suggestions_count_title2, 0));
  }

  public void onDisplayPreferenceDialog(Preference preference) {
    super.onDisplayPreferenceDialog(preference);
  }

  public void onSaveInstanceState(Bundle outState) {}

  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    if (getString(R.string.pref_main_deep_search_key).equals(key)
        || getString(R.string.pref_main_case_sensitive_search_key).equals(key)
        || getString(R.string.pref_settings_special_search_key).equals(key)) {
      mA.mA.handleSearchQuery(false);
    }
  }
}
