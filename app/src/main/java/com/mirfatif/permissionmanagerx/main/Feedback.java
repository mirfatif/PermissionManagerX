package com.mirfatif.permissionmanagerx.main;

import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import androidx.coordinatorlayout.widget.CoordinatorLayout.LayoutParams;
import com.google.android.material.behavior.SwipeDismissBehavior;
import com.google.android.material.behavior.SwipeDismissBehavior.OnDismissListener;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.main.FeedbackDialogFrag.FeedbackType;
import com.mirfatif.permissionmanagerx.prefs.MySettings;

class Feedback {

  private final MainActivity mA;

  Feedback(MainActivity activity) {
    mA = activity;
  }

  private boolean mFeedbackSwiped = false;

  void askForFeedback() {
    if (!mFeedbackSwiped && MySettings.INS.shouldAskForFeedback()) {
      mA.mB.movCont.feedbackCont.setVisibility(View.VISIBLE);
      MySettings.INS.setAskForFeedbackTs(false);
    }

    if (mA.mB.movCont.feedbackCont.getVisibility() != View.VISIBLE) {
      return;
    }

    mA.mB.movCont.likingAppYesButton.setOnClickListener(v -> showDialog(true));
    mA.mB.movCont.likingAppNoButton.setOnClickListener(v -> showDialog(false));

    SwipeDismissBehavior<View> dismissBehavior = new SwipeDismissBehavior<>();
    dismissBehavior.setListener(new FeedbackDismissListener());
    ((LayoutParams) mA.mB.movCont.feedbackCont.getLayoutParams()).setBehavior(dismissBehavior);

    Animation anim = AnimationUtils.loadAnimation(mA.mA, R.anim.shake);
    mA.mB.movCont.feedbackCont.postDelayed(
        () -> mA.mB.movCont.feedbackCont.startAnimation(anim), 1000);
  }

  private void showDialog(boolean isYes) {
    int type = isYes ? FeedbackType.POSITIVE : FeedbackType.NEGATIVE;
    FeedbackDialogFrag.show(type, mA.mA.getSupportFragmentManager());
    mA.mB.movCont.feedbackCont.setVisibility(View.GONE);
  }

  private class FeedbackDismissListener implements OnDismissListener {

    public void onDismiss(View view) {
      mA.mB.movCont.feedbackCont.setVisibility(View.GONE);
      mFeedbackSwiped = true;
    }

    public void onDragStateChanged(int state) {}
  }
}
