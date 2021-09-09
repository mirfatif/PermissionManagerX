package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.main.MainActivity.TAG_DONATION;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.FeedbackDialogBinding;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.AboutActivity;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.util.Utils;

public class FeedbackDialogFrag extends BottomSheetDialogFragment {

  private static final String YES = "IS_YES";

  static FeedbackDialogFrag newInstance(boolean isYes) {
    FeedbackDialogFrag frag = new FeedbackDialogFrag();
    Bundle args = new Bundle();
    args.putBoolean(YES, isYes);
    frag.setArguments(args);
    return frag;
  }

  private MainActivity mA;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mA = (MainActivity) getActivity();
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

    boolean isYes = requireArguments().getBoolean(YES);
    FeedbackDialogBinding b =
        FeedbackDialogBinding.inflate(getLayoutInflater(), container, container != null);

    b.msgV.setText(
        isYes
            ? (BuildConfig.GH_VERSION ? R.string.purchase_and_rate_the_app : R.string.rate_the_app)
            : R.string.ask_to_provide_feedback);
    b.neutralButton.setText(R.string.do_not_ask);
    b.posButton.setText(
        isYes ? (BuildConfig.GH_VERSION ? R.string.i_will : R.string.rate) : R.string.contact);

    b.neutralButton.setOnClickListener(
        v -> {
          MySettings.getInstance().setAskForFeedbackTs(Long.MAX_VALUE);
          dismiss();
        });

    b.posButton.setOnClickListener(
        v -> {
          dismiss();
          if (isYes) {
            if (BuildConfig.GH_VERSION && !BuildConfig.AMAZ_VERSION) {
              AlertDialogFragment.show(mA, null, TAG_DONATION);
            } else {
              Utils.openWebUrl(mA, Utils.getString(R.string.play_store_url));
            }
          } else {
            startActivity(new Intent(App.getContext(), AboutActivity.class));
          }
          Utils.showToast(R.string.thank_you);
        });

    b.negButton.setOnClickListener(v -> dismiss());

    return b.getRoot();
  }

  @Override
  public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
    super.onViewCreated(view, savedInstanceState);

    // Replace the default white background with the custom on which has round corners and
    // background color set.
    // Another option is to override the bottomSheetDialog theme in style.xml
    ((View) view.getParent()).setBackgroundResource(R.drawable.bottom_sheet_background);
  }
}
