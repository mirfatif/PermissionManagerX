package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.ActionBarDrawerToggle;

public class MainActivityFlavor {

  @SuppressWarnings("FieldCanBeLocal")
  private final MainActivity mA;

  private final Feedback mFeedback;

  MainActivityFlavor(MainActivity activity) {
    mA = activity;
    mFeedback = new Feedback(mA);
  }

  @SuppressWarnings("UnusedDeclaration")
  void onCreated(Bundle savedInstanceState) {}

  void onResumed() {
    mFeedback.askForFeedback();
  }

  @SuppressWarnings("UnusedDeclaration")
  void onCreateOptionsMenu(Menu menu) {}

  @SuppressWarnings("UnusedDeclaration")
  boolean onPrepareOptionsMenu(Menu menu) {
    return false;
  }

  @SuppressWarnings("UnusedDeclaration")
  boolean onOptionsItemSelected(MenuItem item) {
    return false;
  }

  @SuppressWarnings("UnusedDeclaration")
  public void setNavMenu(Menu menu) {}

  @SuppressWarnings("UnusedDeclaration")
  public boolean handleNavItemChecked(MenuItem item) {
    return false;
  }

  void onPackagesUpdated() {
    SETTINGS.setMayAskForFeedback();
    mFeedback.askForFeedback();
  }

  void onRestoreDone() {}

  void onPrivDaemonStarted() {}

  public void handleIntentActions(@SuppressWarnings("UnusedDeclaration") Intent intent) {}

  void resetDrawerIcon() {
    ActionBar actionBar = mA.getSupportActionBar();
    ActionBarDrawerToggle drawerToggle;
    if (actionBar != null && (drawerToggle = mA.getDrawerToggle()) != null) {
      actionBar.setHomeAsUpIndicator(drawerToggle.getDrawerArrowDrawable());
    }
  }
}
