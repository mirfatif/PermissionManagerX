package com.mirfatif.permissionmanagerx.prefs;

import static com.mirfatif.permissionmanagerx.parser.PackageParser.PKG_PARSER;
import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.databinding.ActivityFragmentContainerBinding;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Util;

public class FilterSettingsActivity extends BaseActivity {

  private static final String TAG = "FilterSettingsActivity";

  private ActivityFragmentContainerBinding mB;
  private FilterSettingsFragment mFilterSettingsFrag;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    if (Utils.setNightTheme(this)) {
      return;
    }
    mB = ActivityFragmentContainerBinding.inflate(getLayoutInflater());
    setContentView(mB.getRoot());

    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(R.string.filter_menu_item);
    }

    mB.excFiltersMasterSwitch.setVisibility(View.VISIBLE);

    if (SETTINGS.getExcFiltersEnabled()) {
      mB.excFiltersMasterSwitch.setChecked(true);
      addFrag(savedInstanceState);
    }

    // Must use commitNow(). Or the whole settings may get cleared.
    mB.excFiltersMasterSwitch.setOnClickListener(
        v -> {
          if (mB.excFiltersMasterSwitch.isChecked()) {
            // This must be set before setting fragment. Or the whole settings are cleared.
            SETTINGS.setExcFiltersEnabled(mB.excFiltersMasterSwitch.isChecked());
            addFrag(savedInstanceState);
          } else {
            removeFrag();
            // This must be set after removing fragment. Or the whole settings are cleared.
            SETTINGS.setExcFiltersEnabled(mB.excFiltersMasterSwitch.isChecked());
          }
          PKG_PARSER.updatePackagesList();
        });
  }

  private void addFrag(Bundle savedInstanceState) {
    if (savedInstanceState == null) {
      if (mFilterSettingsFrag == null) {
        mFilterSettingsFrag = new FilterSettingsFragment();
      }
      getSupportFragmentManager()
          .beginTransaction()
          .replace(R.id.fragment_container, mFilterSettingsFrag)
          .commitNow();
    }
  }

  private void removeFrag() {
    if (mFilterSettingsFrag != null) {
      getSupportFragmentManager().beginTransaction().remove(mFilterSettingsFrag).commitNow();
    }
  }

  // Override methods of AppCompatActivity related to options menu
  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    // Create menu item(s)
    getMenuInflater().inflate(R.menu.filter_settings, menu);
    if (VERSION.SDK_INT >= VERSION_CODES.P) {
      menu.setGroupDividerEnabled(true);
    }
    menu.findItem(R.id.action_clear_excluded_apps).setEnabled(SETTINGS.getExcludedAppsCount() != 0);
    menu.findItem(R.id.action_clear_excluded_perms)
        .setEnabled(SETTINGS.getExcludedPermsCount() != 0);
    menu.findItem(R.id.action_clear_extra_app_ops).setEnabled(SETTINGS.getExtraAppOpsCount() != 0);
    return true;
  }

  @Override
  public boolean onPrepareOptionsMenu(Menu menu) {
    boolean filtersEnabled = SETTINGS.getExcFiltersEnabled();
    menu.findItem(R.id.action_reset_defaults).setVisible(filtersEnabled);
    menu.findItem(R.id.action_clear_excluded_apps).setVisible(filtersEnabled);
    menu.findItem(R.id.action_clear_excluded_perms).setVisible(filtersEnabled);
    menu.findItem(R.id.action_clear_extra_app_ops).setVisible(filtersEnabled);
    return true;
  }

  /*
   After changes to preferences here, invalidateOptionsMenu() is called after lists are
   updated in FilterSettingsFragment#onSharedPreferenceChanged().
  */
  @Override
  public boolean onOptionsItemSelected(@NonNull MenuItem item) {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "onOptionsItemSelected: " + item.getTitle());
    }

    if (item.getItemId() == R.id.action_reset_defaults) {
      AlertDialogFragment.show(this, null, TAG_RESET_FILTER_SETTINGS);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_excluded_apps) {
      AlertDialogFragment.show(this, null, TAG_CLEAR_EXCLUDED_APPS);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_excluded_perms) {
      AlertDialogFragment.show(this, null, TAG_CLEAR_EXCLUDED_PERMS);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_extra_app_ops) {
      AlertDialogFragment.show(this, null, TAG_CLEAR_EXTRA_APP_OPS);
      return true;
    }

    return super.onOptionsItemSelected(item);
  }

  private static final String CLASS = FilterSettingsActivity.class.getName();
  private static final String TAG_RESET_FILTER_SETTINGS = CLASS + ".RESET_FILTER_SETTINGS";
  private static final String TAG_CLEAR_EXCLUDED_APPS = CLASS + ".CLEAR_EXCLUDED_APPS";
  private static final String TAG_CLEAR_EXCLUDED_PERMS = CLASS + ".CLEAR_EXCLUDED_PERMS";
  private static final String TAG_CLEAR_EXTRA_APP_OPS = CLASS + ".CLEAR_EXTRA_APP_OPS";

  @Override
  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    if (TAG_RESET_FILTER_SETTINGS.equals(tag)) {
      return new Builder(this)
          .setPositiveButton(
              R.string.yes,
              (dialogInterface, i) -> {
                // Clear existing values
                Utils.runInBg(SETTINGS::resetToDefaults);
              })
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_reset_confirmation)
          .create();
    }

    if (TAG_CLEAR_EXCLUDED_APPS.equals(tag)) {
      return new Builder(this)
          .setPositiveButton(R.string.yes, (dialogInterface, i) -> SETTINGS.clearExcludedAppsList())
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_clear_apps_confirmation)
          .create();
    }

    if (TAG_CLEAR_EXCLUDED_PERMS.equals(tag)) {
      return new Builder(this)
          .setPositiveButton(
              R.string.yes, (dialogInterface, i) -> SETTINGS.clearExcludedPermsList())
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_clear_perms_confirmation)
          .create();
    }

    if (TAG_CLEAR_EXTRA_APP_OPS.equals(tag)) {
      return new Builder(this)
          .setPositiveButton(R.string.yes, (dialogInterface, i) -> SETTINGS.clearExtraAppOpsList())
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_clear_app_ops_confirmation)
          .create();
    }

    return super.createDialog(tag, dialogFragment);
  }
}
