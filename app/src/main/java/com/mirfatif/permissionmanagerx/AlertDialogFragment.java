package com.mirfatif.permissionmanagerx;

import android.app.Dialog;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatDialogFragment;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class AlertDialogFragment extends AppCompatDialogFragment {

  static final String TAG = "AlertDialogFragment";
  private final AlertDialog mAlertDialog;

  AlertDialogFragment(AlertDialog alertDialog) {
    mAlertDialog = alertDialog;
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(Bundle savedInstanceState) {
    return mAlertDialog;
  }

  private static final Set<String> allTags = new HashSet<>();
  private final Set<Fragment> mOldDialogs = new HashSet<>();

  synchronized void show(FragmentManager manager, String tag, boolean removeAll) {
    allTags.add(tag);
    mOldDialogs.clear();
    for (String t : (removeAll ? allTags : new HashSet<>(Collections.singleton(tag)))) {
      Fragment fragment = manager.findFragmentByTag(t);
      if (fragment != null) {
        if (MySettings.getInstance().DEBUG) Utils.debugLog(TAG, "Old dialog: " + tag);
        mOldDialogs.add(fragment);
      }
    }

    if (MySettings.getInstance().DEBUG) Utils.debugLog(TAG, "Showing " + tag);

    // If Activity is in background, commitNow throws:
    //   Can not perform this action after onSaveInstanceState
    // We don't have showNowAllowingStateLoss()
    try {
      super.showNow(manager, tag);
    } catch (IllegalStateException e) {
      e.printStackTrace();
    }

    FragmentTransaction transaction = manager.beginTransaction();
    if (MySettings.getInstance().DEBUG) Utils.debugLog(TAG, "Removing old dialogs");
    for (Fragment fragment : mOldDialogs) transaction.remove(fragment);
    transaction.commitNowAllowingStateLoss();
  }
}
