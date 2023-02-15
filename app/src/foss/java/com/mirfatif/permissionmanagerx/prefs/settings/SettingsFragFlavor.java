package com.mirfatif.permissionmanagerx.prefs.settings;

import android.os.Bundle;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.fwk.SettingsActivityM;

public class SettingsFragFlavor extends PreferenceFragmentCompat {

  private SettingsActivityM mA;

  public void onStart() {
    super.onStart();
    mA = (SettingsActivityM) getActivity();
  }

  public void onResume() {
    super.onResume();
    mA.mA.setActionBarTitle(getString(R.string.settings_menu_item));
  }

  private <T extends Preference> T findPref(int key) {
    return findPreference(getString(key));
  }

  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.settings_prefs, rootKey);

    findPref(R.string.pref_settings_general_cat_key)
        .setFragment(SettingsFragGeneral.class.getName());
    findPref(R.string.pref_settings_theming_cat_key).setFragment(SettingsFragTheme.class.getName());
  }
}
