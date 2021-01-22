package com.mirfatif.permissionmanagerx.main;

import android.app.Activity;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.AboutActivity;
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

    boolean isYes = requireArguments().getBoolean(YES);

    View view = inflater.inflate(R.layout.feedback_dialog, container);
    TextView msgView = view.findViewById(R.id.message_view);
    Button neuButton = view.findViewById(R.id.neutral_button);
    Button negButton = view.findViewById(R.id.negative_button);
    Button posButton = view.findViewById(R.id.positive_button);

    msgView.setText(
        isYes
            ? (BuildConfig.GH_VERSION ? R.string.purchase_and_rate_the_app : R.string.rate_the_app)
            : R.string.ask_to_provide_feedback);
    neuButton.setText(R.string.do_not_ask);
    posButton.setText(
        isYes ? (BuildConfig.GH_VERSION ? R.string.i_will : R.string.rate) : R.string.contact);

    neuButton.setOnClickListener(
        v -> {
          MySettings.getInstance().setAskForFeedbackTs(Long.MAX_VALUE);
          dismiss();
        });

    posButton.setOnClickListener(
        v -> {
          dismiss();
          if (isYes) {
            if (BuildConfig.GH_VERSION && !BuildConfig.AMAZ_VERSION) {
              // We are sure this is the MainActivity
              if (mActivity instanceof MainActivity) {
                Donate.showDialog((MainActivity) mActivity);
              }
            } else {
              Utils.openWebUrl(mActivity, Utils.getString(R.string.play_store_url));
            }
          } else {
            startActivity(new Intent(App.getContext(), AboutActivity.class));
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
}
