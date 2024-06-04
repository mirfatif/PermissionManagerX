package com.mirfatif.permissionmanagerx.fwk;

import android.os.Bundle;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import com.mirfatif.permissionmanagerx.base.BaseActivity;
import com.mirfatif.permissionmanagerx.prefs.settings.SettingsActivity;

public class SettingsActivityM extends BaseActivity
    implements PreferenceFragmentCompat.OnPreferenceStartFragmentCallback {

  public final SettingsActivity mA = new SettingsActivity(this);

  protected void onCreated(Bundle savedInstanceState) {
    mA.onCreated(savedInstanceState);
  }

  protected void onSaveInstanceState(Bundle outState) {
    super.onSaveInstanceState(outState);
    mA.onSaveInstanceState(outState);
  }

  public boolean onPreferenceStartFragment(PreferenceFragmentCompat caller, Preference pref) {
    return mA.onPreferenceStartFragment(pref);
  }
}
