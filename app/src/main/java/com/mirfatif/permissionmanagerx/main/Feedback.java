package com.mirfatif.permissionmanagerx.main;

import android.app.Activity;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.SystemClock;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.coordinatorlayout.widget.CoordinatorLayout.LayoutParams;
import com.google.android.material.behavior.SwipeDismissBehavior;
import com.google.android.material.behavior.SwipeDismissBehavior.OnDismissListener;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.AboutActivity;

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
    FeedbackDialogFragFlavor.newInstance(isYes)
        .show(mA.getSupportFragmentManager(), "FEEDBACK_RATING");
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

abstract class FeedbackDialogFrag extends BottomSheetDialogFragment {

  private static final String YES = "IS_YES";

  static FeedbackDialogFragFlavor newInstance(boolean isYes) {
    FeedbackDialogFragFlavor frag = new FeedbackDialogFragFlavor();
    Bundle args = new Bundle();
    args.putBoolean(YES, isYes);
    frag.setArguments(args);
    return frag;
  }

  private Activity mActivity;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mActivity = getActivity();
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
    BottomSheetDialog dialog = (BottomSheetDialog) super.onCreateDialog(savedInstanceState);
    dialog.setDismissWithAnimation(true);
    return dialog;
  }

  @Nullable
  @Override
  public View onCreateView(
      @NonNull LayoutInflater inflater,
      @Nullable ViewGroup container,
      @Nullable Bundle savedInstanceState) {

    // Not possible
    if (getArguments() == null) {
      return null;
    }

    boolean isYes = getArguments().getBoolean(YES);

    View view = inflater.inflate(R.layout.feedback_dialog, container);
    TextView msgView = view.findViewById(R.id.message_view);
    Button neuButton = view.findViewById(R.id.neutral_button);
    Button negButton = view.findViewById(R.id.negative_button);
    Button posButton = view.findViewById(R.id.positive_button);

    msgView.setText(isYes ? getYesMsgResId() : R.string.ask_to_provide_feedback);
    neuButton.setText(getNeutralButtonTextResId());
    posButton.setText(isYes ? getPosButtonYesTextResId() : R.string.contact);

    neuButton.setOnClickListener(
        v -> {
          MySettings.getInstance().setAskForFeedbackTs(Long.MAX_VALUE);
          dismiss();
          neuButtonClicked();
        });

    posButton.setOnClickListener(
        v -> {
          dismiss();
          if (isYes) {
            posButtonYesClicked(mActivity);
          } else {
            startActivity(new Intent(mActivity, AboutActivity.class));
          }
          Toast.makeText(App.getContext(), R.string.thank_you, Toast.LENGTH_LONG).show();
        });

    negButton.setOnClickListener(v -> dismiss());

    return view;
  }

  @Override
  public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
    super.onViewCreated(view, savedInstanceState);

    // Replace the default white background with the custom on which has round corners and
    // background color set.
    // Another option is to override the bottomSheetDialog theme in style.xml
    ((View) view.getParent()).setBackgroundResource(R.drawable.bottom_sheet_background);
  }

  abstract int getYesMsgResId();

  abstract int getPosButtonYesTextResId();

  abstract int getNeutralButtonTextResId();

  abstract void posButtonYesClicked(Activity activity);

  abstract void neuButtonClicked();
}
