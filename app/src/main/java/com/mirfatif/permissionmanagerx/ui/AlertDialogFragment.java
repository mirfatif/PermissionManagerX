package com.mirfatif.permissionmanagerx.ui;

import android.app.Dialog;
import android.content.DialogInterface;
import android.content.DialogInterface.OnDismissListener;
import android.os.Bundle;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatDialogFragment;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;
import com.mirfatif.permissionmanagerx.main.MainActivityFlavor;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.privtasks.Util;
import java.util.HashSet;
import java.util.Set;

public class AlertDialogFragment extends AppCompatDialogFragment {

  private static final String TAG = "AlertDialogFragment";
  private final AlertDialog mAlertDialog;

  public AlertDialogFragment(AlertDialog alertDialog) {
    mAlertDialog = alertDialog;
    MainActivityFlavor.onCreateDialog(alertDialog);
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(Bundle savedInstanceState) {
    return mAlertDialog;
  }

  private static final Set<String> ALL_TAGS = new HashSet<>();

  public void show(FragmentActivity activity, String tag, boolean removeAll) {
    synchronized (AlertDialogFragment.class) {
      FragmentManager manager = activity.getSupportFragmentManager();
      ALL_TAGS.add(tag);

      Set<Fragment> oldDialogs = new HashSet<>();
      if (removeAll) {
        oldDialogs = buildListToRemove(manager);
      } else {
        Fragment fragment = manager.findFragmentByTag(tag);
        if (fragment != null) {
          oldDialogs.add(fragment);
        }
      }

      if (MySettings.getInstance().isDebug()) {
        Util.debugLog(TAG, "Showing " + tag);
      }

      // If Activity is in background, commitNow throws:
      //   "Can not perform this action after onSaveInstanceState"
      // We don't have showNowAllowingStateLoss()
      try {
        if (!activity.isFinishing()
            && !activity.isDestroyed()
            && !activity.isChangingConfigurations()) {
          super.showNow(manager, tag);
        }
      } catch (IllegalStateException e) {
        Log.w(TAG, "show(): " + e.toString());
      }

      removeFragments(manager, oldDialogs);
    }
  }

  public static void removeAll(FragmentActivity activity) {
    FragmentManager manager = activity.getSupportFragmentManager();
    removeFragments(manager, buildListToRemove(manager));
  }

  @Override
  public void onSaveInstanceState(@NonNull Bundle outState) {
    // Do not call super because:
    //  1. We cannot recreate DialogFragment after configuration change.
    //  2. We don't have showNowAllowingStateLoss()
  }

  private static Set<Fragment> buildListToRemove(FragmentManager manager) {
    Set<Fragment> oldDialogs = new HashSet<>();
    Fragment fragment;
    for (String tag : ALL_TAGS) {
      fragment = manager.findFragmentByTag(tag);
      if (fragment != null) {
        if (MySettings.getInstance().isDebug()) {
          Util.debugLog(TAG, "Old dialog: " + tag);
        }
        oldDialogs.add(fragment);
      }
    }
    return oldDialogs;
  }

  private static void removeFragments(FragmentManager manager, Set<Fragment> fragments) {
    FragmentTransaction transaction = manager.beginTransaction();
    if (MySettings.getInstance().isDebug()) {
      Util.debugLog(TAG, "Removing old dialogs");
    }
    for (Fragment fragment : fragments) transaction.remove(fragment);
    transaction.commitNowAllowingStateLoss();
  }

  // We cannot use Dialog's OnDismiss and OnCancel Listeners, DialogFragment owns them.
  private OnDismissListener mDismissListener;

  public AlertDialogFragment setOnDismissListener(OnDismissListener dismissListener) {
    mDismissListener = dismissListener;
    return this;
  }

  @Override
  public void onDismiss(@NonNull DialogInterface dialog) {
    super.onDismiss(dialog);
    if (mDismissListener != null) {
      mDismissListener.onDismiss(dialog);
    }
  }
}
