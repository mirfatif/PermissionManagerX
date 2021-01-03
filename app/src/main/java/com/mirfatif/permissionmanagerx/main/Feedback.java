package com.mirfatif.permissionmanagerx.main;

import android.os.SystemClock;
import android.view.View;
import android.view.animation.AnimationUtils;
import androidx.coordinatorlayout.widget.CoordinatorLayout.LayoutParams;
import com.google.android.material.behavior.SwipeDismissBehavior;
import com.google.android.material.behavior.SwipeDismissBehavior.OnDismissListener;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.prefs.MySettings;

class Feedback {

  private final MainActivity mA;
  private final View mFeedbackContainer;

  Feedback(MainActivity activity) {
    mA = activity;
    mFeedbackContainer = mA.findViewById(R.id.feedback_container);
  }

  void askForFeedback() {
    if (MySettings.getInstance().shouldAskForFeedback()) {
      mFeedbackContainer.setVisibility(View.VISIBLE);
    }

    if (mFeedbackContainer.getVisibility() != View.VISIBLE) {
      return;
    }

    // Undo animation effects from previous swipe.
    mFeedbackContainer.setAlpha(1f);

    mA.findViewById(R.id.liking_app_yes_button).setOnClickListener(v -> showDialog(true));
    mA.findViewById(R.id.liking_app_no_button).setOnClickListener(v -> showDialog(false));

    SwipeDismissBehavior<View> dismissBehavior = new SwipeDismissBehavior<>();
    dismissBehavior.setListener(new FeedbackDismissListener());
    ((LayoutParams) mFeedbackContainer.getLayoutParams()).setBehavior(dismissBehavior);

    Utils.runInBg(
        () -> {
          SystemClock.sleep(1000);
          Utils.runInFg(
              () ->
                  mFeedbackContainer.startAnimation(
                      AnimationUtils.loadAnimation(mA, R.anim.shake)));
        });
  }

  private void showDialog(boolean isYes) {
    FeedbackDialogFrag.newInstance(isYes).show(mA.getSupportFragmentManager(), "FEEDBACK_RATING");
    mFeedbackContainer.setVisibility(View.GONE);
  }

  private class FeedbackDismissListener implements OnDismissListener {

    @Override
    public void onDismiss(View view) {
      mFeedbackContainer.setVisibility(View.GONE);
    }

    @Override
    public void onDragStateChanged(int state) {}
  }
}
