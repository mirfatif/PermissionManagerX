package com.mirfatif.permissionmanagerx.prefs.settings;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.content.Intent;
import android.os.Bundle;
import androidx.activity.OnBackPressedCallback;
import androidx.appcompat.app.ActionBar;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.preference.Preference;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.databinding.ActivityFragmentContainerBinding;
import com.mirfatif.permissionmanagerx.fwk.SettingsActivityM;
import java.util.Objects;

public class SettingsActivity extends OnBackPressedCallback {

  public final SettingsActivityM mA;

  public SettingsActivity(SettingsActivityM activity) {
    super(true);
    mA = activity;
  }

  private static final String CLASS = SettingsActivity.class.getName();
  public static final String EXTRA_NO_PARENT = CLASS + ".extra.NO_PARENT";
  private static final String SAVED_STATE_TITLE = CLASS + ".TITLE";

  public void onCreated(Bundle savedInstanceState) {
    mA.setContentView(ActivityFragmentContainerBinding.inflate(mA.getLayoutInflater()));

    String title = null;

    if (savedInstanceState == null) {
      mA.getSupportFragmentManager()
          .beginTransaction()
          .replace(R.id.fragment_container, new SettingsFrag())
          .commit();
    } else {
      title = savedInstanceState.getString(SAVED_STATE_TITLE);
    }

    super.setEnabled(false);

    ActionBar actionBar = mA.getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(title != null ? title : getString(R.string.settings_menu_item));

      Intent intent = mA.getIntent();
      if (intent != null && intent.getBooleanExtra(EXTRA_NO_PARENT, false)) {
        actionBar.setDisplayHomeAsUpEnabled(false);
      }
    }
  }

  public void handleOnBackPressed() {
    mA.finishAfterTransition();
  }

  public void onSaveInstanceState(Bundle outState) {
    ActionBar actionBar = mA.getSupportActionBar();
    if (actionBar != null) {
      CharSequence title = actionBar.getTitle();
      if (title != null) {
        outState.putString(SAVED_STATE_TITLE, title.toString());
      }
    }
  }

  public boolean onPreferenceStartFragment(Preference pref) {
    FragmentManager fm = mA.getSupportFragmentManager();

    Fragment fragment =
        fm.getFragmentFactory()
            .instantiate(mA.getClassLoader(), Objects.requireNonNull(pref.getFragment()));

    fragment.setArguments(pref.getExtras());

    fm.beginTransaction().replace(R.id.fragment_container, fragment).addToBackStack(null).commit();

    setActionBarTitle(Objects.requireNonNull(pref.getTitle()).toString());

    return true;
  }

  void setActionBarTitle(String title) {
    ActionBar actionBar = mA.getSupportActionBar();

    if (actionBar != null) {
      actionBar.setTitle(title);
    }
  }
}
