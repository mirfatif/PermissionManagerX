package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.content.Intent;
import android.view.Menu;
import android.view.MenuItem;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.ActionBarDrawerToggle;
import androidx.fragment.app.FragmentManager;
import com.mirfatif.permissionmanagerx.main.fwk.MainActivity;

public class MainActivityFlavor {

  @SuppressWarnings("FieldCanBeLocal")
  private final MainActivity mA;

  private final Feedback mFeedback;

  public MainActivityFlavor(MainActivity activity) {
    mA = activity;
    mFeedback = new Feedback(mA);
  }

  public void onCreated() {}

  public void onResumed() {
    mFeedback.askForFeedback();
  }

  @SuppressWarnings("UnusedDeclaration")
  public void onCreateOptionsMenu(Menu menu) {}

  @SuppressWarnings("UnusedDeclaration")
  public boolean onPrepareOptionsMenu(Menu menu) {
    return false;
  }

  @SuppressWarnings("UnusedDeclaration")
  public boolean onOptionsItemSelected(MenuItem item) {
    return false;
  }

  @SuppressWarnings("UnusedDeclaration")
  public void setNavMenu(Menu menu) {}

  @SuppressWarnings("UnusedDeclaration")
  public boolean handleNavItemChecked(MenuItem item) {
    return false;
  }

  public void onPackagesUpdated() {
    SETTINGS.setMayAskForFeedback();
    mFeedback.askForFeedback();
  }

  void onRestoreDone() {}

  public void onPrivDaemonStarted() {}

  public void handleIntentActions(@SuppressWarnings("UnusedDeclaration") Intent intent) {}

  public void resetDrawerIcon() {
    ActionBar actionBar = mA.getSupportActionBar();
    ActionBarDrawerToggle drawerToggle;
    if (actionBar != null && (drawerToggle = mA.getDrawerToggle()) != null) {
      actionBar.setHomeAsUpIndicator(drawerToggle.getDrawerArrowDrawable());
    }
  }

  public int hideSearchSettings(@SuppressWarnings("UnusedDeclaration") FragmentManager fm) {
    return 0;
  }

  public void onBackPressed() {}
}
