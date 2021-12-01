package com.mirfatif.permissionmanagerx.prefs.settings;

import androidx.preference.Preference;
import com.mirfatif.permissionmanagerx.main.fwk.MainActivity;

public class SearchSettingsFragFlavor {

  SearchSettingsFragFlavor(@SuppressWarnings("UnusedDeclaration") SearchSettingsFrag prefFrag) {}

  void onCreatePreferences(@SuppressWarnings("UnusedDeclaration") MainActivity activity) {}

  boolean onSharedPreferenceChanged(@SuppressWarnings("UnusedDeclaration") String key) {
    return false;
  }

  public boolean onDisplayPreferenceDialog(
      @SuppressWarnings("UnusedDeclaration") Preference preference) {
    return false;
  }
}
