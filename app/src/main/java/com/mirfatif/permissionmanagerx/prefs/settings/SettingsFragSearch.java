package com.mirfatif.permissionmanagerx.prefs.settings;

import android.os.Bundle;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;

public class SettingsFragSearch extends PreferenceFragmentCompat {

  @Override
  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.settings_prefs_search, rootKey);
  }
}
