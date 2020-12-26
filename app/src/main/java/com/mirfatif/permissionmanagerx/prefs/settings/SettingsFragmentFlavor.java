package com.mirfatif.permissionmanagerx.prefs.settings;

import android.os.Bundle;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;

// OnSharedPreferenceChangeListener must be global to avoid GC
public class SettingsFragmentFlavor extends PreferenceFragmentCompat {

  @Override
  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.settings_prefs_flavor, rootKey);
  }
}
