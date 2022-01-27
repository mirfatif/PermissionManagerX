package com.mirfatif.permissionmanagerx.prefs;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentActivity;
import androidx.preference.MultiSelectListPreferenceDialogFragmentCompat;
import com.mirfatif.permissionmanagerx.util.Utils;

public class MultiSelectListPrefDialogFrag extends MultiSelectListPreferenceDialogFragmentCompat {

  public static MultiSelectListPrefDialogFrag newInstance(String prefKey) {
    MultiSelectListPrefDialogFrag fragment = new MultiSelectListPrefDialogFrag();
    Bundle args = new Bundle();
    args.putString(ARG_KEY, prefKey);
    fragment.setArguments(args);
    return fragment;
  }

  private FragmentActivity mA;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mA = getActivity();
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(Bundle savedInstanceState) {
    Dialog dialog = super.onCreateDialog(savedInstanceState);
    Utils.onCreateDialog(dialog, mA);
    return dialog;
  }
}
