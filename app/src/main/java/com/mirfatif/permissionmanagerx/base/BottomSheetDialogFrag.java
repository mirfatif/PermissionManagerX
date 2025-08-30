package com.mirfatif.permissionmanagerx.base;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import androidx.fragment.app.FragmentActivity;
import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;

public class BottomSheetDialogFrag extends BottomSheetDialogFragment {

  public FragmentActivity mA;

  public void onAttach(Context context) {
    super.onAttach(context);
    mA = getActivity();
  }

  public Dialog onCreateDialog(Bundle savedInstanceState) {
    BottomSheetDialog dialog = (BottomSheetDialog) super.onCreateDialog(savedInstanceState);
    dialog.setDismissWithAnimation(true);
    dialog.setOnShowListener(
        d -> {
          View view = dialog.findViewById(com.google.android.material.R.id.design_bottom_sheet);
          if (view != null) {
            BottomSheetBehavior.from(view).setState(BottomSheetBehavior.STATE_EXPANDED);
          }
        });
    return dialog;
  }

  public void onViewCreated(View view, Bundle savedInstanceState) {
    super.onViewCreated(view, savedInstanceState);

    ((View) view.getParent()).setBackground(new DialogBg(true, mA));
  }

  public int getTheme() {
    return com.google.android.material.R.style.Theme_Design_BottomSheetDialog;
  }
}
