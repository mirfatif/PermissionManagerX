package com.mirfatif.permissionmanagerx.main;

import android.view.Menu;
import android.view.MenuItem;
import com.mirfatif.permissionmanagerx.prefs.MySettings;

public class MainActivityFlavor {

  @SuppressWarnings("FieldCanBeLocal")
  private final MainActivity mA;

  private final Feedback mFeedback;
  private final MySettings mMySettings = MySettings.getInstance();

  MainActivityFlavor(MainActivity activity) {
    mA = activity;
    mFeedback = new Feedback(mA);
  }

  @SuppressWarnings("UnusedDeclaration")
  void onCreated() {}

  void onResumed() {
    mFeedback.askForFeedback();
  }

  void onDestroyed() {}

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

  void onPackagesUpdated() {
    mMySettings.setMayAskForFeedback();
    mFeedback.askForFeedback();
  }

  void onRestoreDone() {}

  void onPrivDaemonStarted() {}
}
