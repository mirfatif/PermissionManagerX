package com.mirfatif.permissionmanagerx.ui.base;

import android.app.Dialog;
import android.content.Context;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.View;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.res.ResourcesCompat;
import androidx.fragment.app.FragmentActivity;
import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.mirfatif.permissionmanagerx.R;

public class BottomSheetDialogFrag extends BottomSheetDialogFragment {

  public FragmentActivity mA;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mA = getActivity();
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
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

  @Override
  public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
    super.onViewCreated(view, savedInstanceState);
    /*
     Replace the default white background with the custom one.
     Another option is to override the bottomSheetDialog theme in style.xml
     With setBackgroundResource() background color is not set, so first create drawable.
    */
    int resId = R.drawable.bottom_sheet_bg_flavor;
    Drawable bg = ResourcesCompat.getDrawable(mA.getResources(), resId, mA.getTheme());
    ((View) view.getParent()).setBackground(bg);
  }

  // Required for correct Buttons background/text color.
  @Override
  public int getTheme() {
    return R.style.Theme_Design_BottomSheetDialog;
  }
}
