package com.mirfatif.permissionmanagerx.main;

import android.content.Intent;
import android.view.Menu;
import android.view.MenuItem;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.ActionBarDrawerToggle;

class MainActivityFlavor {

  private final MainActivity mA;

  private final Feedback mFeedback;

  MainActivityFlavor(MainActivity activity) {
    mA = activity;
    mFeedback = new Feedback(mA);
  }

  void onCreated() {}

  void onResumed() {
    mFeedback.askForFeedback();
  }

  void onCreateOptionsMenu(Menu menu) {}

  boolean onPrepareOptionsMenu(Menu menu) {
    return false;
  }

  boolean onOptionsItemSelected(MenuItem item) {
    return false;
  }

  void setNavMenu(Menu menu) {}

  boolean handleNavItemChecked(MenuItem item) {
    return false;
  }

  void onPackagesUpdated(int pkgCount) {}

  void askForFeedback() {
    mFeedback.askForFeedback();
  }

  void onRestoreDone() {}

  void onPrivDaemonStarted() {}

  void handleIntentActions(Intent intent) {}

  void preOpenDrawer() {}

  void resetDrawerIcon() {
    ActionBar actionBar = mA.mA.getSupportActionBar();
    ActionBarDrawerToggle drawerToggle;
    if (actionBar != null && (drawerToggle = mA.mDrawerToggle) != null) {
      actionBar.setHomeAsUpIndicator(drawerToggle.getDrawerArrowDrawable());
    }
  }

  void onBackPressed() {}

  void onRootStopped() {}

  void onAdbStopped() {}

  void onNotifPermGranted() {}
}
