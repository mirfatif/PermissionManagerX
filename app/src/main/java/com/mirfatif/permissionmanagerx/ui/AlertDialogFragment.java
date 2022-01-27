package com.mirfatif.permissionmanagerx.ui;

import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnDismissListener;
import android.os.Bundle;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.app.AppCompatDialogFragment;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle.State;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Util;

public class AlertDialogFragment extends AppCompatDialogFragment {

  private static final String TAG = "AlertDialogFragment";

  public static final String DIALOG_TAG = AlertDialogFragment.class.getName() + ".0";

  public AlertDialogFragment() {}

  private AlertDialog mAlertDialog;

  public AlertDialogFragment(AlertDialog alertDialog) {
    mAlertDialog = alertDialog;
  }

  private BaseActivity mA;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mA = (BaseActivity) getActivity();
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(Bundle savedInstanceState) {
    if (mAlertDialog == null) {
      mAlertDialog = mA.createDialog(requireArguments().getString(DIALOG_TAG), this);
      if (mAlertDialog == null) {
        dismissAllowingStateLoss();
        return new Builder(mA).create();
      }
    }
    Utils.onCreateDialog(mAlertDialog, mA);
    return mAlertDialog;
  }

  // We cannot use Dialog's OnDismiss and OnCancel Listeners, DialogFragment owns them.
  private OnDismissListener mDismissListener;

  // Must be set again if Fragment is recreated.
  @SuppressWarnings("UnusedReturnValue")
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

  public static AlertDialogFragment show(
      FragmentActivity activity, AlertDialog alertDialog, String tag) {
    synchronized (AlertDialogFragment.class) {
      FragmentManager manager = activity.getSupportFragmentManager();
      Fragment fragment = manager.findFragmentByTag(tag);
      if (fragment != null) {
        manager.beginTransaction().remove(fragment).commitNowAllowingStateLoss();
      }

      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "Showing " + tag);
      }

      AlertDialogFragment frag = new AlertDialogFragment(alertDialog);
      Bundle args = new Bundle();
      args.putString(DIALOG_TAG, tag);
      frag.setArguments(args);

      /*
       If Activity is in background, commitNow throws:
         "Can not perform this action after onSaveInstanceState"
       We don't have showNowAllowingStateLoss()
      */
      try {
        if (activity.getLifecycle().getCurrentState().isAtLeast(State.INITIALIZED)
            && !activity.isChangingConfigurations()) {
          frag.showNow(manager, tag);
        }
      } catch (IllegalStateException e) {
        Log.w(TAG, "show: " + e.toString());
      }
      return frag;
    }
  }
}
