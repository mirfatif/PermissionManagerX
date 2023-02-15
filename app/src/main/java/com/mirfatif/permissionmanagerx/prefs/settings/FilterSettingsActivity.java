package com.mirfatif.permissionmanagerx.prefs.settings;

import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.ActivityFragmentContainerBinding;
import com.mirfatif.permissionmanagerx.fwk.FilterSettingsActivityM;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.privtasks.util.bg.BgRunner;

public class FilterSettingsActivity {

  public final FilterSettingsActivityM mA;

  public FilterSettingsActivity(FilterSettingsActivityM activity) {
    mA = activity;
  }

  private ActivityFragmentContainerBinding mB;
  private FilterSettingsFragment mFilterSettingsFrag;

  public void onCreated(Bundle savedInstanceState) {
    mB = ActivityFragmentContainerBinding.inflate(mA.getLayoutInflater());
    mA.setContentView(mB.getRoot());

    ActionBar actionBar = mA.getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(R.string.filter_menu_item);
    }

    mB.excFiltersMasterSwitch.setVisibility(View.VISIBLE);

    if (MySettings.INS.getExcFiltersEnabled()) {
      mB.excFiltersMasterSwitch.setChecked(true);
      addFrag(savedInstanceState);
    }

    mB.excFiltersMasterSwitch.setOnClickListener(
        v -> {
          if (mB.excFiltersMasterSwitch.isChecked()) {

            MySettings.INS.setExcFiltersEnabled(true);
            addFrag(savedInstanceState);
          } else {
            removeFrag();

            MySettings.INS.setExcFiltersEnabled(false);
          }
          PackageParser.INS.updatePkgList();
        });
  }

  private void addFrag(Bundle savedInstanceState) {
    if (savedInstanceState == null) {
      if (mFilterSettingsFrag == null) {
        mFilterSettingsFrag = new FilterSettingsFragment();
      }
      mA.getSupportFragmentManager()
          .beginTransaction()
          .replace(R.id.fragment_container, mFilterSettingsFrag)
          .commitNow();
    }
  }

  private void removeFrag() {
    if (mFilterSettingsFrag != null) {
      mA.getSupportFragmentManager().beginTransaction().remove(mFilterSettingsFrag).commitNow();
    }
  }

  public boolean onCreateOptionsMenu(Menu menu) {
    mA.getMenuInflater().inflate(R.menu.filter_settings, menu);
    if (VERSION.SDK_INT >= VERSION_CODES.P) {
      menu.setGroupDividerEnabled(true);
    }
    menu.findItem(R.id.action_clear_excluded_apps)
        .setEnabled(ExcFiltersData.INS.getExcludedAppsCount() != 0);
    menu.findItem(R.id.action_clear_excluded_perms)
        .setEnabled(ExcFiltersData.INS.getExcludedPermsCount() != 0);
    menu.findItem(R.id.action_clear_extra_app_ops)
        .setEnabled(ExcFiltersData.INS.getExtraAppOpsCount() != 0);
    return true;
  }

  public boolean onPrepareOptionsMenu(Menu menu) {
    boolean filtersEnabled = MySettings.INS.getExcFiltersEnabled();
    menu.findItem(R.id.action_reset_defaults).setVisible(filtersEnabled);
    menu.findItem(R.id.action_clear_excluded_apps).setVisible(filtersEnabled);
    menu.findItem(R.id.action_clear_excluded_perms).setVisible(filtersEnabled);
    menu.findItem(R.id.action_clear_extra_app_ops).setVisible(filtersEnabled);
    return true;
  }

  public boolean onOptionsItemSelected(MenuItem item) {
    if (item.getItemId() == R.id.action_reset_defaults) {
      AlertDialogFragment.show(mA, null, TAG_RESET_FILTER_SETTINGS);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_excluded_apps) {
      AlertDialogFragment.show(mA, null, TAG_CLEAR_EXCLUDED_APPS);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_excluded_perms) {
      AlertDialogFragment.show(mA, null, TAG_CLEAR_EXCLUDED_PERMS);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_extra_app_ops) {
      AlertDialogFragment.show(mA, null, TAG_CLEAR_EXTRA_APP_OPS);
      return true;
    }

    return false;
  }

  private static final String CLASS = FilterSettingsActivity.class.getName();
  private static final String TAG_RESET_FILTER_SETTINGS = CLASS + ".RESET_FILTER_SETTINGS";
  private static final String TAG_CLEAR_EXCLUDED_APPS = CLASS + ".CLEAR_EXCLUDED_APPS";
  private static final String TAG_CLEAR_EXCLUDED_PERMS = CLASS + ".CLEAR_EXCLUDED_PERMS";
  private static final String TAG_CLEAR_EXTRA_APP_OPS = CLASS + ".CLEAR_EXTRA_APP_OPS";

  public AlertDialog createDialog(String tag) {
    if (TAG_RESET_FILTER_SETTINGS.equals(tag)) {
      return new Builder(mA)
          .setPositiveButton(
              R.string.yes, (d, w) -> BgRunner.execute(ExcFiltersData.INS::resetExcFilters))
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_reset_confirmation)
          .create();
    }

    if (TAG_CLEAR_EXCLUDED_APPS.equals(tag)) {
      return new Builder(mA)
          .setPositiveButton(R.string.yes, (d, w) -> MySettings.INS.clearExcludedAppsList())
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_clear_apps_confirmation)
          .create();
    }

    if (TAG_CLEAR_EXCLUDED_PERMS.equals(tag)) {
      return new Builder(mA)
          .setPositiveButton(R.string.yes, (d, w) -> MySettings.INS.clearExcludedPermsList())
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_clear_perms_confirmation)
          .create();
    }

    if (TAG_CLEAR_EXTRA_APP_OPS.equals(tag)) {
      return new Builder(mA)
          .setPositiveButton(R.string.yes, (d, w) -> MySettings.INS.clearExtraAppOpsList())
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_clear_app_ops_confirmation)
          .create();
    }

    return null;
  }
}
