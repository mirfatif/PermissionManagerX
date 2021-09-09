package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.os.SystemClock;
import android.view.View;
import android.view.animation.AnimationUtils;
import androidx.coordinatorlayout.widget.CoordinatorLayout.LayoutParams;
import com.google.android.material.behavior.SwipeDismissBehavior;
import com.google.android.material.behavior.SwipeDismissBehavior.OnDismissListener;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.databinding.ActivityMainBinding;
import com.mirfatif.permissionmanagerx.util.Utils;

class Feedback {

  private final MainActivity mA;
  private final ActivityMainBinding mB;

  Feedback(MainActivity activity) {
    mA = activity;
    mB = mA.getRootView();
  }

  // Resuming visibility and alpha of the Feedback container after it's
  // swiped away is buggy. So we show the container only once per Activity launch.
  private boolean mFeedbackSwiped = false;

  void askForFeedback() {
    if (!mFeedbackSwiped && SETTINGS.shouldAskForFeedback()) {
      mB.movCont.feedbackCont.setVisibility(View.VISIBLE);
    }

    if (mB.movCont.feedbackCont.getVisibility() != View.VISIBLE) {
      return;
    }

    mB.movCont.likingAppYesButton.setOnClickListener(v -> showDialog(true));
    mB.movCont.likingAppNoButton.setOnClickListener(v -> showDialog(false));

    SwipeDismissBehavior<View> dismissBehavior = new SwipeDismissBehavior<>();
    dismissBehavior.setListener(new FeedbackDismissListener());
    ((LayoutParams) mB.movCont.feedbackCont.getLayoutParams()).setBehavior(dismissBehavior);

    Utils.runInBg(
        () -> {
          SystemClock.sleep(1000);
          Utils.runInFg(
              () ->
                  mB.movCont.feedbackCont.startAnimation(
                      AnimationUtils.loadAnimation(mA, R.anim.shake)));
        });
  }

  private void showDialog(boolean isYes) {
    FeedbackDialogFrag.newInstance(isYes).show(mA.getSupportFragmentManager(), "FEEDBACK_RATING");
    mB.movCont.feedbackCont.setVisibility(View.GONE);
  }

  private class FeedbackDismissListener implements OnDismissListener {

    @Override
    public void onDismiss(View view) {
      mB.movCont.feedbackCont.setVisibility(View.GONE);
      mFeedbackSwiped = true;
    }

    @Override
    public void onDragStateChanged(int state) {}
  }
}
