package com.mirfatif.permissionmanagerx.main;

import android.content.Intent;
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
  void onCreated(Intent intent) {}

  void onCreateOptionsMenu() {}

  void onResumed() {
    mFeedback.askForFeedback();
  }

  void onDestroyed() {}

  void onPackagesUpdated() {
    mMySettings.setMayAskForFeedback();
    mFeedback.askForFeedback();
  }

  void onRestoreDone() {}
}
