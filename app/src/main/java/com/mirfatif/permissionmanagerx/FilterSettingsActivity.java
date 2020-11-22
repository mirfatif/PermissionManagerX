package com.mirfatif.permissionmanagerx;

import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;
import java.util.HashSet;
import java.util.Objects;

public class FilterSettingsActivity extends AppCompatActivity {

  private MySettings mMySettings;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_filter_settings);

    Objects.requireNonNull(getSupportActionBar()).setTitle(R.string.filter_menu_item);

    mMySettings = MySettings.getInstance();

    FragmentManager fragmentManager = getSupportFragmentManager();
    FragmentTransaction transaction = fragmentManager.beginTransaction();
    transaction.replace(R.id.filter_settings_container, new FilterSettingsFragment());
    transaction.commit();
  }

  // Override methods of AppCompatActivity related to options menu
  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    // Create menu item(s)
    getMenuInflater().inflate(R.menu.filter_settings, menu);
    menu.findItem(R.id.action_clear_excluded_apps)
        .setEnabled(mMySettings.getExcludedAppsCount() != 0);
    menu.findItem(R.id.action_clear_excluded_perms)
        .setEnabled(mMySettings.getExcludedPermsCount() != 0);
    menu.findItem(R.id.action_clear_extra_app_ops)
        .setEnabled(mMySettings.getExtraAppOpsCount() != 0);
    return super.onCreateOptionsMenu(menu);
  }

  /**
   * After changes to preferences here, {@link #invalidateOptionsMenu()} is called after lists are
   * updated in {@link FilterSettingsFragment#onSharedPreferenceChanged(SharedPreferences, String)}
   */
  @Override
  public boolean onOptionsItemSelected(@NonNull MenuItem item) {
    if (mMySettings.DEBUG)
      Utils.debugLog("FilterSettingsActivity", "onOptionsItemSelected(): " + item.getTitle());
    if (item.getItemId() == R.id.action_reset_defaults) {
      // Build an AlertDialog and set listeners on buttons
      AlertDialog.Builder builder = new AlertDialog.Builder(this);
      builder.setPositiveButton(
          R.string.yes,
          (dialogInterface, i) -> {
            // clear existing values
            Utils.runInBg(() -> mMySettings.resetToDefaults());
          });

      builder.setNegativeButton(R.string.no, null);

      // Set message, create and show the AlertDialog
      builder.setTitle(R.string.filter_settings);
      builder.setMessage(R.string.filter_settings_reset_confirmation);
      AlertDialog alertDialog = builder.create();
      alertDialog.show();
      return true;
    }

    if (item.getItemId() == R.id.action_clear_excluded_apps) {
      // Build an AlertDialog and set listeners on buttons
      AlertDialog.Builder builder = new AlertDialog.Builder(this);
      builder.setPositiveButton(
          R.string.yes,
          (dialogInterface, i) ->
              mMySettings.savePref(R.string.filter_settings_excluded_apps_key, new HashSet<>()));

      builder.setNegativeButton(R.string.no, null);

      // Set message, create and show the AlertDialog
      builder.setTitle(R.string.filter_settings);
      builder.setMessage(R.string.filter_settings_clear_apps_confirmation);
      AlertDialog alertDialog = builder.create();
      alertDialog.show();
      return true;
    }

    if (item.getItemId() == R.id.action_clear_excluded_perms) {
      AlertDialog.Builder builder = new AlertDialog.Builder(this);
      builder.setPositiveButton(
          R.string.yes,
          (dialogInterface, i) ->
              mMySettings.savePref(R.string.filter_settings_excluded_perms_key, new HashSet<>()));

      builder.setNegativeButton(R.string.no, null);
      builder.setTitle(R.string.filter_settings);
      builder.setMessage(R.string.filter_settings_clear_perms_confirmation);
      AlertDialog alertDialog = builder.create();
      alertDialog.show();
      return true;
    }

    if (item.getItemId() == R.id.action_clear_extra_app_ops) {
      new AlertDialog.Builder(this)
          .setPositiveButton(
              R.string.yes,
              (dialogInterface, i) ->
                  mMySettings.savePref(R.string.filter_settings_extra_appops_key, new HashSet<>()))
          .setNegativeButton(R.string.no, null)
          .setTitle(R.string.filter_settings)
          .setMessage(R.string.filter_settings_clear_app_ops_confirmation)
          .create()
          .show();
      return true;
    }

    // do not recreate parent (Main) activity
    if (item.getItemId() == android.R.id.home) {
      onBackPressed();
      return true;
    }

    return super.onOptionsItemSelected(item);
  }
}