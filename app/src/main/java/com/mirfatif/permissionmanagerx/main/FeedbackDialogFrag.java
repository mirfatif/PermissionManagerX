package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.main.fwk.MainActivity.TAG_DONATION;
import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;
import static com.mirfatif.permissionmanagerx.util.Utils.isProVersion;
import static com.mirfatif.permissionmanagerx.util.Utils.isPsVersion;
import static com.mirfatif.permissionmanagerx.util.Utils.openWebUrl;

import android.os.Bundle;
import android.text.format.DateUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.databinding.FeedbackDialogBinding;
import com.mirfatif.permissionmanagerx.databinding.RateDonateDialogBinding;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.ui.base.BottomSheetDialogFrag;
import com.mirfatif.permissionmanagerx.util.Utils;

public class FeedbackDialogFrag extends BottomSheetDialogFrag {

  private static final String FEEDBACK_TYPE = "FEEDBACK_TYPE";

  public enum FeedbackType {
    POSITIVE,
    NEGATIVE,
    RATE,
    RATE_DONATE,
    CONTACT
  }

  public static void show(FeedbackType type, FragmentManager fm) {
    FeedbackDialogFrag frag = new FeedbackDialogFrag();
    Bundle args = new Bundle();
    args.putString(FEEDBACK_TYPE, type.name());
    frag.setArguments(args);
    frag.show(fm, "FEEDBACK_RATING");
  }

  @Nullable
  @Override
  public View onCreateView(
      @NonNull LayoutInflater inflater,
      @Nullable ViewGroup container,
      @Nullable Bundle savedInstanceState) {

    FeedbackType type = FeedbackType.valueOf(requireArguments().getString(FEEDBACK_TYPE));
    if (type == FeedbackType.POSITIVE) {
      return getFeedbackView(true);
    }
    if (type == FeedbackType.NEGATIVE) {
      return getFeedbackView(false);
    }
    return getButtonsView(type);
  }

  private View getFeedbackView(boolean isYes) {
    FeedbackDialogBinding b = FeedbackDialogBinding.inflate(mA.getLayoutInflater());
    int msgResId, buttonResId;

    if (isYes) {
      if (isPsVersion() || isProVersion()) {
        msgResId = R.string.rate_the_app;
        buttonResId = R.string.rate_now;
      } else {
        msgResId = R.string.purchase_and_rate_the_app;
        buttonResId = R.string.how_do_i;
      }
    } else {
      msgResId = R.string.ask_to_provide_feedback;
      buttonResId = R.string.contact;
    }

    b.msgV.setText(msgResId);
    b.posButton.setText(buttonResId);

    b.neutralButton.setOnClickListener(
        v -> {
          SETTINGS.setAskForFeedbackTs(System.currentTimeMillis() + DateUtils.WEEK_IN_MILLIS * 25);
          dismiss();
        });

    b.negButton.setOnClickListener(v -> dismiss());

    b.posButton.setOnClickListener(
        v -> {
          FeedbackType type = isYes ? FeedbackType.RATE_DONATE : FeedbackType.CONTACT;
          FeedbackDialogFrag.show(type, mA.getSupportFragmentManager());
          dismiss();
        });

    return b.getRoot();
  }

  private View getButtonsView(FeedbackType type) {
    int b1 = 0, b2 = 0, b3 = 0;
    ButtonListener l1 = null, l2 = null, l3 = null;

    RateDonateDialogBinding b = RateDonateDialogBinding.inflate(mA.getLayoutInflater());

    if (type == FeedbackType.CONTACT) {
      b1 = R.string.contact_on_telegram;
      l1 = new ButtonListener(() -> openWebUrl(mA, Utils.getString(R.string.telegram_mirfatif)));
      b2 = R.string.contact_on_email;
      l2 = new ButtonListener(() -> Utils.sendMail(mA, null));
    } else {
      if (isPsVersion()) {
        b1 = R.string.rate_on_ps;
        l1 = new ButtonListener(() -> openWebUrl(mA, getString(R.string.play_store_url)));
      } else {
        if (type == FeedbackType.RATE_DONATE && !isProVersion()) {
          b1 = R.string.purchase_donate;
          l1 = new ButtonListener(() -> AlertDialogFragment.show(mA, null, TAG_DONATION));
        }
        b2 = R.string.review_on_xda;
        l2 = new ButtonListener(() -> openWebUrl(mA, getString(R.string.xda_url)));
      }

      b3 = R.string.star_on_github;
      l3 = new ButtonListener(() -> openWebUrl(mA, Utils.getString(R.string.source_url)));
    }

    if (l1 != null) {
      b.button1.setText(b1);
      b.button1.setOnClickListener(l1);
    } else {
      b.button1.setVisibility(View.GONE);
    }

    if (l2 != null) {
      b.button2.setText(b2);
      b.button2.setOnClickListener(l2);
    } else {
      b.button2.setVisibility(View.GONE);
    }

    if (l3 != null) {
      b.button3.setText(b3);
      b.button3.setOnClickListener(l3);
    } else {
      b.button3.setVisibility(View.GONE);
    }

    return b.getRoot();
  }

  private class ButtonListener implements OnClickListener {

    private final Runnable mTask;

    private ButtonListener(Runnable task) {
      mTask = task;
    }

    @Override
    public void onClick(View v) {
      mTask.run();
      Utils.showToast(R.string.thank_you);
      dismiss();
    }
  }
}
