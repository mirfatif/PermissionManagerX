package com.mirfatif.permissionmanagerx.prefs.settings;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.main.fwk.MainActivity;
import com.mirfatif.permissionmanagerx.util.Utils;

public class SearchSettingsFrag extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private final SearchSettingsFragFlavor mSettingsFlavor;

  public SearchSettingsFrag() {
    mSettingsFlavor = new SearchSettingsFragFlavor(this);
  }

  private MainActivity mA;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mA = (MainActivity) getActivity();
    Utils.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
  }

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
    setPreferencesFromResource(R.xml.settings_prefs_search_flavor, rootKey);
    mSettingsFlavor.onCreatePreferences(mA);
  }

  @Override
  public void onDisplayPreferenceDialog(Preference preference) {
    if (!mSettingsFlavor.onDisplayPreferenceDialog(preference)) {
      super.onDisplayPreferenceDialog(preference);
    }
  }

  @Override
  public void onSaveInstanceState(@NonNull Bundle outState) {}

  @Override
  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    if (getString(R.string.pref_main_deep_search_key).equals(key)
        || getString(R.string.pref_main_case_sensitive_search_key).equals(key)
        || getString(R.string.pref_settings_special_search_key).equals(key)
        || mSettingsFlavor.onSharedPreferenceChanged(key)) {
      mA.handleSearchQuery(false);
    }
  }
}
