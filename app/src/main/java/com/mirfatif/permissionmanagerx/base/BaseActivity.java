package com.mirfatif.permissionmanagerx.base;

import android.content.Context;
import android.content.res.Configuration;
import android.os.Bundle;
import android.view.MenuItem;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AppCompatDelegate;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.util.LocaleUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.UiUtilsFlavor;

public abstract class BaseActivity extends AppCompatActivity {

  protected synchronized void onCreate(Bundle savedInstanceState) {
    UiUtilsFlavor.onCreateStart(this);
    super.onCreate(savedInstanceState);

    if (!setNightTheme()) {
      onCreated(savedInstanceState);
    }
  }

  protected abstract void onCreated(Bundle savedInstanceState);

  public boolean onOptionsItemSelected(MenuItem item) {

    if (item.getItemId() == android.R.id.home) {
      onBackPressed();
      return true;
    }
    return super.onOptionsItemSelected(item);
  }

  protected void attachBaseContext(Context context) {
    super.attachBaseContext(LocaleUtils.setLocale(context));
  }

  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(LocaleUtils.setLocale(newConfig));
  }

  public void applyOverrideConfiguration(Configuration overrideConfiguration) {
    super.applyOverrideConfiguration(LocaleUtils.setLocale(overrideConfiguration));
  }

  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    return null;
  }

  private boolean setNightTheme() {
    boolean wasNightMode = UiUtils.isNightMode(this);

    String mode = MySettings.INS.getDarkThemeMode();
    if (mode.equals(getString(R.string.dark_theme_mode_light_val))) {
      AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_NO);
    } else if (mode.equals(getString(R.string.dark_theme_mode_dark_val))) {
      AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES);
    } else {
      AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
    }

    return wasNightMode != UiUtils.isNightMode(this);
  }
}
