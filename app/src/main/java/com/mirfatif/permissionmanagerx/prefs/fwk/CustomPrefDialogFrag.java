package com.mirfatif.permissionmanagerx.prefs.fwk;

import android.os.Bundle;
import androidx.fragment.app.DialogFragment;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.preference.EditTextPreference;
import androidx.preference.ListPreference;
import androidx.preference.MultiSelectListPreference;
import androidx.preference.Preference;
import com.mirfatif.permissionmanagerx.util.ApiUtils;

public class CustomPrefDialogFrag {

  private CustomPrefDialogFrag() {}

  private static final String TAG_EDIT_TEXT_DIALOG =
      EditTextPrefDialogFrag.class.getName() + ".EDIT_TEXT_DIALOG";

  private static final String TAG_LIST_DIALOG =
      EditTextPrefDialogFrag.class.getName() + ".LIST_DIALOG";

  private static final String TAG_MULTI_LIST_DIALOG =
      EditTextPrefDialogFrag.class.getName() + ".MULTI_LIST_DIALOG";

  public static boolean showPrefDialogFrag(Preference preference, Fragment fragment) {
    DialogFragment frag;
    String ARG_KEY, TAG;

    if (preference instanceof EditTextPreference) {
      EditTextPrefDialogFrag newFrag = new EditTextPrefDialogFrag();
      frag = newFrag;
      ARG_KEY = newFrag.getArgKey();
      TAG = TAG_EDIT_TEXT_DIALOG;

    } else if (preference instanceof ListPreference) {
      ListPrefDialogFrag newFrag = new ListPrefDialogFrag();
      frag = newFrag;
      ARG_KEY = newFrag.getArgKey();
      TAG = TAG_LIST_DIALOG;

    } else if (preference instanceof MultiSelectListPreference) {
      MultiSelectListPrefDialogFrag newFrag = new MultiSelectListPrefDialogFrag();
      frag = newFrag;
      ARG_KEY = newFrag.getArgKey();
      TAG = TAG_MULTI_LIST_DIALOG;

    } else {
      return false;
    }

    FragmentManager fm = fragment.getParentFragmentManager();

    if (fm.findFragmentByTag(TAG) != null) {
      return true;
    }

    Bundle args = new Bundle();
    args.putString(ARG_KEY, preference.getKey());

    frag.setArguments(args);
    ApiUtils.setTargetFragment(frag, fragment);
    frag.show(fm, TAG);

    return true;
  }
}
