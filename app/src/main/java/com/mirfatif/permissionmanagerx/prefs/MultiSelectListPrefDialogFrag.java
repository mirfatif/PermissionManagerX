package com.mirfatif.permissionmanagerx.prefs;

import android.app.Dialog;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.preference.MultiSelectListPreferenceDialogFragmentCompat;
import com.mirfatif.permissionmanagerx.util.UtilsFlavor;

public class MultiSelectListPrefDialogFrag extends MultiSelectListPreferenceDialogFragmentCompat {

  public static MultiSelectListPrefDialogFrag newInstance(String prefKey) {
    MultiSelectListPrefDialogFrag fragment = new MultiSelectListPrefDialogFrag();
    Bundle args = new Bundle();
    args.putString(ARG_KEY, prefKey);
    fragment.setArguments(args);
    return fragment;
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(Bundle savedInstanceState) {
    Dialog dialog = super.onCreateDialog(savedInstanceState);
    UtilsFlavor.onCreateDialog(dialog);
    return dialog;
  }
}
