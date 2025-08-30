package com.mirfatif.permissionmanagerx.base;

import android.content.Context;
import android.content.res.Configuration;
import android.os.Build;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.ViewGroup;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.viewbinding.ViewBinding;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.databinding.StatusBarBgContBinding;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.util.LocaleUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public abstract class BaseActivity extends AppCompatActivity {

  protected synchronized void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    if (!setNightTheme()) {
      onCreated(savedInstanceState);
    }
  }

  protected abstract void onCreated(Bundle savedInstanceState);

  public boolean onOptionsItemSelected(MenuItem item) {
    if (item.getItemId() == android.R.id.home) {
      getOnBackPressedDispatcher().onBackPressed();
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

  public void setContentView(ViewBinding binding) {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.VANILLA_ICE_CREAM) {
      setContentView(binding.getRoot());
      return;
    }

    var cont = StatusBarBgContBinding.inflate(getLayoutInflater());
    setContentView(cont.getRoot());
    cont.getRoot().addView(binding.getRoot(), cont.getRoot().getLayoutParams());

    WindowCompat.getInsetsController(getWindow(), getWindow().getDecorView())
        .setAppearanceLightStatusBars(!UiUtils.isNightMode(this));

    ViewCompat.setOnApplyWindowInsetsListener(
        cont.getRoot(),
        (v, insets) -> {
          var type = WindowInsetsCompat.Type.systemBars() | WindowInsetsCompat.Type.displayCutout();
          var ins = insets.getInsets(type);

          var mlp = (ViewGroup.MarginLayoutParams) v.getLayoutParams();
          mlp.leftMargin = ins.left;
          mlp.bottomMargin = ins.bottom;
          mlp.rightMargin = ins.right;

          ActionBar actionBar = getSupportActionBar();
          if (actionBar != null) {
            actionBar.setBackgroundDrawable(null);
            var lp = cont.statusBarBg.getLayoutParams();
            lp.height = ins.top;
            cont.statusBarBg.setLayoutParams(lp);
          } else {
            mlp.topMargin = ins.top;
          }

          v.setLayoutParams(mlp);

          return WindowInsetsCompat.CONSUMED;
        });
  }
}
