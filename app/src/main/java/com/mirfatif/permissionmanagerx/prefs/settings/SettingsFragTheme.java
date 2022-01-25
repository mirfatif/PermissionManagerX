package com.mirfatif.permissionmanagerx.prefs.settings;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentActivity;
import androidx.preference.ListPreference.SimpleSummaryProvider;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.Utils;

@SuppressWarnings("UnusedDeclaration")
public class SettingsFragTheme extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private FragmentActivity mA;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    Utils.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
    mA = getActivity();
  }

  @Override
  public void onPause() {
    super.onPause();
    Utils.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
  }

  @Override
  public void onResume() {
    super.onResume();
    Utils.getDefPrefs().registerOnSharedPreferenceChangeListener(this);
  }

  @Override
  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.settings_prefs_theming, rootKey);
    Preference pref = findPreference(getString(R.string.pref_settings_theme_color_key));
    if (pref != null) {
      pref.setEnabled(!Utils.isFreeVersion());
      pref.setSummaryProvider(SimpleSummaryProvider.getInstance());
    }
  }

  @Override
  public void onDisplayPreferenceDialog(Preference preference) {
    if (!ListPrefDialogFrag.startFragment(preference, this)) {
      super.onDisplayPreferenceDialog(preference);
    }
  }

  @Override
  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    if (key.equals(getString(R.string.pref_settings_theme_color_key))
        || key.equals(getString(R.string.pref_settings_dark_theme_key))) {
      mA.recreate();
      SETTINGS.recreateMainActivity();
    }
  }
}
