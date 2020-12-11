package com.mirfatif.permissionmanagerx;

import android.content.Intent;
import android.widget.Toast;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;

class MainActivityFlavor {

  static final boolean SHOW_ACTION_DONATE = true;
  static final boolean SHOW_ACTION_SETTINGS = false;

  private final MainActivity mActivity;

  MainActivityFlavor(MainActivity activity) {
    mActivity = activity;
  }

  void askForRating() {
    if (mActivity.mMySettings.shouldNotAskForRating()) return;
    AlertDialog dialog =
        new Builder(mActivity)
            .setMessage(R.string.purchase_and_rate_the_app)
            .setPositiveButton(
                android.R.string.ok,
                (d, which) -> {
                  Utils.openWebUrl(mActivity, mActivity.getString(R.string.play_store_url));
                  Toast.makeText(App.getContext(), R.string.thank_you, Toast.LENGTH_LONG).show();
                })
            .setNegativeButton(android.R.string.cancel, null)
            .setNeutralButton(
                R.string.shut_up,
                (d, which) -> {
                  mActivity.mMySettings.setAskForRatingTs(Long.MAX_VALUE);
                  Toast.makeText(App.getContext(), "\ud83d\ude1f", Toast.LENGTH_LONG).show();
                })
            .create();
    new AlertDialogFragment(dialog).show(mActivity.mFM, "RATING", false);
  }

  @SuppressWarnings("UnusedDeclaration")
  void onCreated(Intent intent) {}

  void onCreateOptionsMenu() {}

  void onResumed() {}

  void onDestroyed() {}

  void openSettings() {}

  void onPackagesUpdated() {}
}
