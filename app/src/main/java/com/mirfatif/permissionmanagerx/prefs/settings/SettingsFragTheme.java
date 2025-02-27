package com.mirfatif.permissionmanagerx.prefs.settings;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import androidx.fragment.app.FragmentActivity;
import androidx.preference.ListPreference.SimpleSummaryProvider;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.fwk.CustomPrefDialogFrag;
import java.util.Objects;

public class SettingsFragTheme extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private FragmentActivity mA;

  public void onAttach(Context context) {
    super.onAttach(context);
    mA = getActivity();
  }

  public void onPause() {
    super.onPause();
    MySettings.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
  }

  public void onResume() {
    super.onResume();
    MySettings.getDefPrefs().registerOnSharedPreferenceChangeListener(this);
  }

  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.settings_prefs_theming, rootKey);
    Preference pref = findPreference(getString(R.string.pref_settings_theme_color_key));
    if (pref != null) {
      pref.setEnabled(false);
      pref.setSummaryProvider(SimpleSummaryProvider.getInstance());
    }
  }

  public void onDisplayPreferenceDialog(Preference preference) {
    if (!CustomPrefDialogFrag.showPrefDialogFrag(preference, this)) {
      super.onDisplayPreferenceDialog(preference);
    }
  }

  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    if (Objects.requireNonNull(key).equals(getString(R.string.pref_settings_theme_color_key))
        || key.equals(getString(R.string.pref_settings_dark_theme_key))) {
      mA.recreate();
      MySettings.INS.recreateMainActivity();
    }
  }
}
