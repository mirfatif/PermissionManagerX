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
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.parser.SearchConstants;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.fwk.CustomPrefDialogFrag;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import java.util.Objects;

public class SettingsFragGeneral extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private FragmentActivity mA;

  public void onAttach(Context context) {
    super.onAttach(context);
    mA = getActivity();
  }

  public void onPause() {
    MySettings.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
    super.onPause();
  }

  public void onResume() {
    super.onResume();
    MySettings.getDefPrefs().registerOnSharedPreferenceChangeListener(this);
  }

  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.settings_prefs_general, rootKey);
    Preference pref = findPreference(ApiUtils.getString(R.string.pref_settings_locale_key));
    if (pref != null) {
      pref.setSummaryProvider(SimpleSummaryProvider.getInstance());
    }

    SettingsFragGeneralFlavor.onCreatePreferences(this);
  }

  public void onDisplayPreferenceDialog(Preference preference) {
    if (!CustomPrefDialogFrag.showPrefDialogFrag(preference, this)) {
      super.onDisplayPreferenceDialog(preference);
    }
  }

  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    if (Objects.requireNonNull(key).equals(ApiUtils.getString(R.string.pref_settings_locale_key))) {
      App.setLocale();
      SearchConstants.INS.recreate();
      mA.recreate();
      MySettings.INS.recreateMainActivity();
      PackageParser.INS.updatePkgList();
    } else {
      SettingsFragGeneralFlavor.onSharedPreferenceChanged(key);
    }
  }
}
