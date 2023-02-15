package com.mirfatif.permissionmanagerx.prefs.fwk;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import androidx.fragment.app.FragmentActivity;
import androidx.preference.MultiSelectListPreferenceDialogFragmentCompat;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class MultiSelectListPrefDialogFrag extends MultiSelectListPreferenceDialogFragmentCompat {

  private FragmentActivity mA;

  public void onAttach(Context context) {
    super.onAttach(context);
    mA = getActivity();
  }

  public Dialog onCreateDialog(Bundle savedInstanceState) {
    Dialog dialog = super.onCreateDialog(savedInstanceState);
    UiUtils.onCreateDialog(dialog, mA);
    return dialog;
  }

  String getArgKey() {
    return ARG_KEY;
  }
}
