package com.mirfatif.permissionmanagerx.prefs.settings;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.preference.ListPreference;
import androidx.preference.ListPreferenceDialogFragmentCompat;
import androidx.preference.Preference;
import com.mirfatif.permissionmanagerx.util.Utils;

public class ListPrefDialogFrag extends ListPreferenceDialogFragmentCompat {

  public static ListPrefDialogFrag newInstance(String prefKey) {
    ListPrefDialogFrag fragment = new ListPrefDialogFrag();
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

  @SuppressWarnings("BooleanMethodIsAlwaysInverted")
  public static boolean startFragment(Preference preference, Fragment fragment) {
    // We need to override ListPreference with a custom-built
    // DialogFragment in order to customize AlertDialog.
    if (preference instanceof ListPreference) {
      final String TAG_LIST_DIALOG = "LIST_DIALOG";

      FragmentManager fm = fragment.getParentFragmentManager();
      if (fm.findFragmentByTag(TAG_LIST_DIALOG) == null) {
        ListPrefDialogFrag newFrag = newInstance(preference.getKey());
        setTargetFragment(newFrag, fragment);
        newFrag.show(fm, TAG_LIST_DIALOG);
      }
      return true;
    }
    return false;
  }

  // Without this onCreate() crashes because getTargetFragment() returns null
  @SuppressWarnings("deprecation")
  private static void setTargetFragment(Fragment source, Fragment target) {
    source.setTargetFragment(target, 0);
  }
}
