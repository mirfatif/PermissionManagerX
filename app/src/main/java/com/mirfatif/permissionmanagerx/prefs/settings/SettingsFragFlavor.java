package com.mirfatif.permissionmanagerx.prefs.settings;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.preference.ListPreference.SimpleSummaryProvider;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.main.MainActivity;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.parser.SearchConstants;
import com.mirfatif.permissionmanagerx.util.Utils;

@SuppressWarnings("UnusedDeclaration")
public class SettingsFragFlavor extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
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
    setPreferencesFromResource(R.xml.settings_prefs_flavor, rootKey);
    Preference pref = findPreference(getString(R.string.pref_settings_locale_key));
    if (pref != null) {
      pref.setSummaryProvider(SimpleSummaryProvider.getInstance());
    }
  }

  @Override
  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    if (key.equals(getString(R.string.pref_settings_quick_scan_key))) {
      PackageParser.getInstance().updatePackagesList();
    } else if (key.equals(getString(R.string.pref_settings_locale_key))) {
      App.updateContext();
      SearchConstants.recreate();
      MainActivity.restart();
    }
  }
}
