package com.mirfatif.permissionmanagerx.prefs;

import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.fragment.app.FragmentManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.privtasks.Util;

public class FilterSettingsActivity extends BaseActivity {

  private MySettings mMySettings;
  private final FragmentManager mFM = getSupportFragmentManager();

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_fragment_container);

    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(R.string.filter_menu_item);
    }

    mMySettings = MySettings.getInstance();

    if (savedInstanceState == null) {
      getSupportFragmentManager()
          .beginTransaction()
          .replace(R.id.fragment_container, new FilterSettingsFragment())
          .commit();
    }
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
    if (mMySettings.isDebug()) {
      Util.debugLog("FilterSettingsActivity", "onOptionsItemSelected(): " + item.getTitle());
    }
    if (item.getItemId() == R.id.action_reset_defaults) {
      // Build an AlertDialog and set listeners on buttons
      Builder builder = new Builder(this);
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
      new AlertDialogFragment(builder.create()).show(mFM, "RESET_FILTER_SETTINGS", false);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_excluded_apps) {
      // Build an AlertDialog and set listeners on buttons
      Builder builder = new Builder(this);
      builder.setPositiveButton(
          R.string.yes, (dialogInterface, i) -> mMySettings.clearExcludedAppsList());

      builder.setNegativeButton(R.string.no, null);

      // Set message, create and show the AlertDialog
      builder.setTitle(R.string.filter_settings);
      builder.setMessage(R.string.filter_settings_clear_apps_confirmation);
      new AlertDialogFragment(builder.create()).show(mFM, "CLEAR_EXCLUDED_APPS", false);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_excluded_perms) {
      Builder builder = new Builder(this);
      builder.setPositiveButton(
          R.string.yes, (dialogInterface, i) -> mMySettings.clearExcludedPermsList());

      builder.setNegativeButton(R.string.no, null);
      builder.setTitle(R.string.filter_settings);
      builder.setMessage(R.string.filter_settings_clear_perms_confirmation);
      new AlertDialogFragment(builder.create()).show(mFM, "CLEAR_EXCLUDED_PERMS", false);
      return true;
    }

    if (item.getItemId() == R.id.action_clear_extra_app_ops) {
      AlertDialog dialog =
          new Builder(this)
              .setPositiveButton(
                  R.string.yes, (dialogInterface, i) -> mMySettings.clearExtraAppOpsList())
              .setNegativeButton(R.string.no, null)
              .setTitle(R.string.filter_settings)
              .setMessage(R.string.filter_settings_clear_app_ops_confirmation)
              .create();
      new AlertDialogFragment(dialog).show(mFM, "CLEAR_EXTRA_APP_OPS", false);
      return true;
    }

    return super.onOptionsItemSelected(item);
  }
}
