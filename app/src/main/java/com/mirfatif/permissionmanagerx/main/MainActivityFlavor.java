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

  @SuppressWarnings("UnusedDeclaration")
  void onCreateOptionsMenu(Menu menu) {}

  void onResumed() {
    mFeedback.askForFeedback();
  }

  void onDestroyed() {}

  void onPackagesUpdated() {
    mMySettings.setMayAskForFeedback();
    mFeedback.askForFeedback();
  }

  void onRestoreDone() {}

  @SuppressWarnings("UnusedDeclaration")
  public boolean onOptionsItemSelected(MenuItem item) {
    return false;
  }
}
