package com.mirfatif.permissionmanagerx.prefs.settings;

import android.os.Bundle;
import android.view.MenuItem;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.privtasks.Util;

public class SettingsActivity extends AppCompatActivity {

  @Override
  protected void onCreate(@Nullable Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_fragment_container);

    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) actionBar.setTitle(R.string.settings_menu_item);

    // Check null to avoid:
    // "IllegalStateException: Target fragment must implement TargetFragment interface"
    // on rotation when a DialogPreference is visible.
    // https://issuetracker.google.com/issues/137173772
    if (savedInstanceState == null) {
      getSupportFragmentManager()
          .beginTransaction()
          .replace(R.id.fragment_container, new SettingsFragmentFlavor())
          .commit();
    }
  }

  @Override
  public boolean onOptionsItemSelected(@NonNull MenuItem item) {
    if (MySettings.getInstance().isDebug())
      Util.debugLog("SettingsActivity", "onOptionsItemSelected(): " + item.getTitle());

    // do not recreate parent (Main) activity
    if (item.getItemId() == android.R.id.home) {
      onBackPressed();
      return true;
    }

    return super.onOptionsItemSelected(item);
  }
}
