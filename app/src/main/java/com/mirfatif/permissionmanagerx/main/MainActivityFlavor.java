package com.mirfatif.permissionmanagerx.main;

import android.app.Activity;
import android.app.Dialog;
import android.content.Intent;
import android.view.View;
import android.view.ViewGroup;
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

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateDialog(Dialog dialog) {}

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateStart(Activity activity) {}

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateLayout(ViewGroup viewGroup) {}

  public static void onSnackBarSwiped(View view) {
    view.setTranslationY(0);
  }

  @SuppressWarnings("UnusedDeclaration")
  public static void onSnackBarMoved(View view) {}
}
