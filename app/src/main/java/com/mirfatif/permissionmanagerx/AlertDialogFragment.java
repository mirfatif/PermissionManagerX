package com.mirfatif.permissionmanagerx;

import android.app.Dialog;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatDialogFragment;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;
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

  synchronized void show(FragmentManager manager, String tag, boolean removeAll) {
    allTags.add(tag);

    Set<Fragment> oldDialogs = new HashSet<>();
    if (removeAll) oldDialogs = buildListToRemove(manager);
    else {
      Fragment fragment = manager.findFragmentByTag(tag);
      if (fragment != null) oldDialogs.add(fragment);
    }

    if (MySettings.getInstance().DEBUG) Utils.debugLog(TAG, "Showing " + tag);

    // If Activity is in background, commitNow throws:
    //   "Can not perform this action after onSaveInstanceState"
    // We don't have showNowAllowingStateLoss()
    try {
      super.showNow(manager, tag);
    } catch (IllegalStateException e) {
      e.printStackTrace();
    }

    removeFragments(manager, oldDialogs);
  }

  static void removeAll(FragmentManager manager) {
    removeFragments(manager, buildListToRemove(manager));
  }

  private static Set<Fragment> buildListToRemove(FragmentManager manager) {
    Set<Fragment> oldDialogs = new HashSet<>();
    Fragment fragment;
    for (String tag : allTags) {
      fragment = manager.findFragmentByTag(tag);
      if (fragment != null) {
        if (MySettings.getInstance().DEBUG) Utils.debugLog(TAG, "Old dialog: " + tag);
        oldDialogs.add(fragment);
      }
    }
    return oldDialogs;
  }

  private static void removeFragments(FragmentManager manager, Set<Fragment> fragments) {
    FragmentTransaction transaction = manager.beginTransaction();
    if (MySettings.getInstance().DEBUG) Utils.debugLog(TAG, "Removing old dialogs");
    for (Fragment fragment : fragments) transaction.remove(fragment);
    transaction.commitNowAllowingStateLoss();
  }
}
