package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.content.Intent;
import android.view.Menu;
import android.view.MenuItem;

public class MainActivityFlavor {

  @SuppressWarnings("FieldCanBeLocal")
  private final MainActivity mA;

  private final Feedback mFeedback;

  MainActivityFlavor(MainActivity activity) {
    mA = activity;
    mFeedback = new Feedback(mA);
  }

  @SuppressWarnings("UnusedDeclaration")
  void onCreated() {}

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
}
