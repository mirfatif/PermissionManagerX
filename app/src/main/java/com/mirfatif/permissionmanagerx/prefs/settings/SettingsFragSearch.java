package com.mirfatif.permissionmanagerx.prefs.settings;

import static com.mirfatif.permissionmanagerx.parser.PackageParser.PKG_PARSER;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.Utils;

public class SettingsFragSearch extends PreferenceFragmentCompat
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
    setPreferencesFromResource(R.xml.settings_prefs_search, rootKey);
  }

  @Override
  public void onSaveInstanceState(@NonNull Bundle outState) {}

  @Override
  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    /*
     Redo search if deep search or case-sensitivity changed.
     No need to call MainActivity#handleSearchQuery()
    */
    PKG_PARSER.handleSearchQuery();
  }
}
