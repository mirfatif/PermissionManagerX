package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.openWebUrl;
import static com.mirfatif.permissionmanagerx.util.Utils.isPsProVersion;
import static com.mirfatif.permissionmanagerx.util.Utils.isSelfProVersion;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import androidx.fragment.app.FragmentManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.about.AboutActivity;
import com.mirfatif.permissionmanagerx.base.BottomSheetDialogFrag;
import com.mirfatif.permissionmanagerx.databinding.FeedbackDialogBinding;
import com.mirfatif.permissionmanagerx.databinding.RateDonateDialogBinding;
import com.mirfatif.permissionmanagerx.help.HelpActivity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class FeedbackDialogFrag extends BottomSheetDialogFrag {

  private static final String FEEDBACK_TYPE = "FEEDBACK_TYPE";

  @Retention(RetentionPolicy.SOURCE)
  public @interface FeedbackType {
    int POSITIVE = 0;
    int NEGATIVE = 1;
    int RATE = 2;
    int RATE_DONATE = 3;
    int CONTACT = 4;
  }

  public static void show(int type, FragmentManager fm) {
    FeedbackDialogFrag frag = new FeedbackDialogFrag();
    Bundle args = new Bundle();
    args.putInt(FEEDBACK_TYPE, type);
    frag.setArguments(args);
    frag.show(fm, "FEEDBACK_RATING");
  }

  public View onCreateView(
      LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

    int type = requireArguments().getInt(FEEDBACK_TYPE);
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
      if (isPsProVersion() || isSelfProVersion()) {
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
          MySettings.INS.setAskForFeedbackTs(true);
          dismissAllowingStateLoss();
        });

    b.negButton.setOnClickListener(v -> dismissAllowingStateLoss());

    b.posButton.setOnClickListener(
        v -> {
          int type = isYes ? FeedbackType.RATE_DONATE : FeedbackType.CONTACT;
          FeedbackDialogFrag.show(type, mA.getSupportFragmentManager());
          dismissAllowingStateLoss();
        });

    return b.getRoot();
  }

  private View getButtonsView(int type) {
    int b1 = 0, b2, b3 = 0;
    ButtonListener l1 = null, l2, l3 = null;

    RateDonateDialogBinding b = RateDonateDialogBinding.inflate(mA.getLayoutInflater());

    if (type == FeedbackType.CONTACT) {
      b1 = R.string.contact_on_telegram;
      l1 = new ButtonListener(() -> openWebUrl(mA, ApiUtils.getString(R.string.telegram_mirfatif)));
      b2 = R.string.contact_on_email;
      l2 = new ButtonListener(() -> ApiUtils.sendMail(mA, null));

    } else {
      b2 = R.string.rating_options;

      if (isPsProVersion() || type == FeedbackType.RATE) {
        b1 = R.string.rate_on_ps;
        l1 = new ButtonListener(() -> openWebUrl(mA, getString(R.string.play_store_url)));

        b2 = R.string.other_rating_options;

      } else if (type == FeedbackType.RATE_DONATE && !isSelfProVersion()) {
        b1 = R.string.purchase_donate;
        l1 =
            new ButtonListener(() -> ApiUtils.openWebUrl(mA, getString(R.string.purchase_pro_url)));
      }

      l2 =
          new ButtonListener(
              () -> HelpActivity.start(mA, getString(R.string.rate_review_help_href)));

      if (type == FeedbackType.RATE_DONATE) {
        b3 = R.string.share_with_others;
        l3 = new ButtonListener(() -> AboutActivity.sendShareIntent(mA));
      }
    }

    if (l1 != null) {
      b.button1.setText(b1);
      b.button1.setOnClickListener(l1);
    } else {
      b.button1.setVisibility(View.GONE);
    }

    b.button2.setText(b2);
    b.button2.setOnClickListener(l2);

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

    public void onClick(View v) {
      mTask.run();
      UiUtils.showToast(R.string.thank_you);
      dismissAllowingStateLoss();
    }
  }
}
