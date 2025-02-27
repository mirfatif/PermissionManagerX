package com.mirfatif.permissionmanagerx.prefs.settings;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import androidx.preference.Preference;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.fwk.MainActivityM;
import java.util.Objects;

public class SearchSettingsFragFlavor {

  private final SearchSettingsFrag mPrefFrag;

  SearchSettingsFragFlavor(SearchSettingsFrag prefFrag) {
    mPrefFrag = prefFrag;
  }

  void onCreatePreferences(MainActivityM activity) {
    Preference pref =
        mPrefFrag.findPreference(getString(R.string.pref_settings_search_suggestions_count_key));
    Objects.requireNonNull(pref)
        .setTitle(getString(R.string.pref_settings_search_suggestions_count_title2, 0));
  }

  public boolean onDisplayPreferenceDialog(Preference preference) {
    return false;
  }
}
