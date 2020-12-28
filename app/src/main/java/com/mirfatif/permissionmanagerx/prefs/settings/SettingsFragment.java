package com.mirfatif.permissionmanagerx.prefs.settings;

import android.os.Bundle;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;

public class SettingsFragment extends PreferenceFragmentCompat {

  @Override
  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.settings_prefs_flavor, rootKey);
  }

  @Override
  public void onResume() {
    super.onResume();
    SettingsActivity activity = (SettingsActivity) getActivity();
    if (activity != null) {
      activity.setActionBarTitle(getString(R.string.settings_menu_item));
    }
  }
}
