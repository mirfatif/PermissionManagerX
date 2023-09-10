package com.mirfatif.permissionmanagerx.prefs.settings;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.widget.EditText;
import androidx.preference.EditTextPreference;
import androidx.preference.ListPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.SwitchPreferenceCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.fwk.AdvSettingsActivityM;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.fwk.CustomPrefDialogFrag;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.privs.DaemonStarter;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.io.File;
import java.util.Objects;

public class AdvSettingsFrag extends PreferenceFragmentCompat
    implements OnSharedPreferenceChangeListener {

  private final AdvSettingsFlavor mAdvFlav = new AdvSettingsFlavor();

  private AdvSettingsActivityM mA;

  public void onAttach(Context context) {
    super.onAttach(context);
    mA = (AdvSettingsActivityM) getActivity();
  }

  public void onPause() {
    super.onPause();
    MySettings.getDefPrefs().unregisterOnSharedPreferenceChangeListener(this);
  }

  public void onResume() {
    super.onResume();
    MySettings.getDefPrefs().registerOnSharedPreferenceChangeListener(this);
  }

  private <T extends Preference> T findPref(int keyResId) {
    return findPreference(ApiUtils.getString(keyResId));
  }

  private SwitchPreferenceCompat mAllowCriticalPref, mPerUidPermRefPref, mSecUserPermRefPref;
  private ListPreference mDaemonDexLocPref, mDaemonUidPref, mDaemonContextPref;
  private EditTextPreference mSuPathPref;

  private boolean mAllowCriticalValue, mPerUidPermRefValue, mSecUserPermRefValue;
  private String mDaemonDexLocVal, mDaemonUidValue, mDaemonContextValue, mSuPathValue;

  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    setPreferencesFromResource(R.xml.adv_settings_prefs, rootKey);
    mAdvFlav.onCreatePrefs(mA, this);

    mAllowCriticalPref = findPref(R.string.pref_adv_settings_allow_critical_changes_key);
    mAllowCriticalValue = mAllowCriticalPref.isChecked();

    mPerUidPermRefPref = findPref(R.string.pref_adv_settings_unique_ref_app_op_uid_mode_key);
    mPerUidPermRefValue = mPerUidPermRefPref.isChecked();

    mSecUserPermRefPref = findPref(R.string.pref_adv_settings_unique_ref_sec_users_key);
    mSecUserPermRefValue = mSecUserPermRefPref.isChecked();

    Preference pref = findPref(R.string.pref_adv_settings_reset_perm_db_key);
    if (Utils.isFreeVersion()) {
      pref.setVisible(true);
      pref.setEnabled(DaemonHandler.INS.isDaemonAlive());
      pref.setOnPreferenceClickListener(
          p -> {
            mA.mA.showPermDbResetDialog();
            return true;
          });
    }

    mDaemonDexLocPref = findPref(R.string.pref_adv_settings_daemon_dex_location_key);
    mDaemonDexLocVal = mDaemonDexLocPref.getValue();

    mDaemonUidPref = findPref(R.string.pref_adv_settings_daemon_uid_key);
    mDaemonUidValue = mDaemonUidPref.getValue();

    mDaemonContextPref = findPref(R.string.pref_adv_settings_daemon_context_key);
    mDaemonContextValue = mDaemonContextPref.getValue();

    mSuPathPref = findPref(R.string.pref_adv_settings_su_exe_path_key);
    mSuPathValue = mSuPathPref.getText();
    mSuPathPref.setOnBindEditTextListener(
        editText -> {
          editText.setHint(R.string.su_exe_path_hint);
          editText.addTextChangedListener(new SuPathWatcher(editText));
        });

    updateUi();
  }

  private void updateUi() {
    mDaemonDexLocPref.setSummary(
        ApiUtils.getString(
            R.string.pref_adv_settings_daemon_dex_location_summary, mDaemonDexLocPref.getEntry()));
    mDaemonUidPref.setTitle(
        ApiUtils.getString(
            R.string.pref_adv_settings_daemon_uid_title2, mDaemonUidPref.getEntry()));
    mDaemonContextPref.setTitle(
        ApiUtils.getString(
            R.string.pref_adv_settings_daemon_context_title2, mDaemonContextPref.getEntry()));
    String suPath = mSuPathPref.getText();
    mSuPathPref.setSummary(
        TextUtils.isEmpty(suPath) ? ApiUtils.getString(R.string.su_exe_path_hint) : suPath);
  }

  public void onDisplayPreferenceDialog(Preference preference) {
    if (!CustomPrefDialogFrag.showPrefDialogFrag(preference, this)) {
      super.onDisplayPreferenceDialog(preference);
    }
  }

  private static boolean isNotExecutableFile(CharSequence path) {
    File file = new File(path.toString());
    return !file.isFile() || !file.canExecute();
  }

  private static class SuPathWatcher implements TextWatcher {

    private final EditText mSuPathEditText;

    private SuPathWatcher(EditText editText) {
      mSuPathEditText = editText;
    }

    public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

    public void onTextChanged(CharSequence s, int start, int before, int count) {
      if (mSuPathEditText != null && !TextUtils.isEmpty(s) && isNotExecutableFile(s)) {
        mSuPathEditText.setError(ApiUtils.getString(R.string.bad_path_toast), null);
      }
    }

    public void afterTextChanged(Editable s) {}
  }

  public void onDestroy() {
    if (!Objects.equals(mDaemonDexLocVal, mDaemonDexLocPref.getValue())
        || !Objects.equals(mDaemonUidValue, mDaemonUidPref.getValue())
        || !Objects.equals(mDaemonContextValue, mDaemonContextPref.getValue())
        || !Objects.equals(mSuPathValue, mSuPathPref.getText())) {
      DaemonStarter.INS.startPrivDaemon(true, false, true, true);
    } else if (mAllowCriticalValue != mAllowCriticalPref.isChecked()
        || mPerUidPermRefValue != mPerUidPermRefPref.isChecked()
        || mSecUserPermRefValue != mSecUserPermRefPref.isChecked()) {
      PackageParser.INS.updatePkgList();
    }
    super.onDestroy();
  }

  public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    if (Objects.requireNonNull(key)
        .equals(ApiUtils.getString(R.string.pref_adv_settings_exit_on_app_death_key))) {
      if (DaemonHandler.INS.isDaemonAlive()) {
        BgRunner.execute(
            () -> {
              NativeDaemon.INS_A.setExitOnAppDeath();
              NativeDaemon.INS_R.setExitOnAppDeath();
              DaemonIface.INS.setExitOnAppDeath();
            });
      }
    } else if (key.equals(ApiUtils.getString(R.string.pref_adv_settings_su_exe_path_key))) {
      String su = mSuPathPref.getText();
      if (!TextUtils.isEmpty(su) && isNotExecutableFile(Objects.requireNonNull(su))) {
        mSuPathPref.setText(mSuPathValue);
        UiUtils.showToast(R.string.bad_path_toast);
      }
    } else {
      mAdvFlav.onPrefChanged(key);
    }
    updateUi();
  }
}
