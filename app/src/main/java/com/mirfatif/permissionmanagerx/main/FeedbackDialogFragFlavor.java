package com.mirfatif.permissionmanagerx.main;

import android.app.Activity;
import android.widget.Toast;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;

public class FeedbackDialogFragFlavor extends FeedbackDialogFrag {

  @Override
  int getYesMsgResId() {
    return R.string.purchase_and_rate_the_app;
  }

  @Override
  int getPosButtonYesTextResId() {
    return R.string.i_will;
  }

  @Override
  int getNeutralButtonTextResId() {
    return R.string.shut_up;
  }

  @Override
  void posButtonYesClicked(Activity activity) {
    // We are sure this is the MainActivity
    if (activity instanceof MainActivity) {
      Donate.showDialog((MainActivity) activity);
    }
  }

  @Override
  void neuButtonClicked() {
    Toast.makeText(App.getContext(), "\ud83d\ude1f", Toast.LENGTH_LONG).show();
  }
}
