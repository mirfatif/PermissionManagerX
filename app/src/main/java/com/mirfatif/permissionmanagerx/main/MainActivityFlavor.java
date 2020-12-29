package com.mirfatif.permissionmanagerx.main;

import android.content.Intent;
import android.widget.Toast;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;

public class MainActivityFlavor {

  private final MainActivity mActivity;
  private final MySettings mMySettings = MySettings.getInstance();

  MainActivityFlavor(MainActivity activity) {
    mActivity = activity;
  }

  private void askForRating() {
    if (mMySettings.shouldNotAskForRating()) {
      return;
    }
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
                  mMySettings.setAskForRatingTs(Long.MAX_VALUE);
                  Toast.makeText(App.getContext(), "\ud83d\ude1f", Toast.LENGTH_LONG).show();
                })
            .create();
    new AlertDialogFragment(dialog).show(mActivity.getSupportFragmentManager(), "RATING", false);
  }

  @SuppressWarnings("UnusedDeclaration")
  void onCreated(Intent intent) {}

  void onCreateOptionsMenu() {}

  void onResumed() {}

  void onDestroyed() {}

  void onPackagesUpdated() {
    askForRating();
  }

  void onRestoreDone() {}
}
