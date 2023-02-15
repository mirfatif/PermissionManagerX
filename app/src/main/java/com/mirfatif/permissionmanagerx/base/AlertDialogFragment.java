package com.mirfatif.permissionmanagerx.base;

import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnDismissListener;
import android.os.Bundle;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.app.AppCompatDialogFragment;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle.State;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.privtasks.util.MyLog;

public class AlertDialogFragment extends AppCompatDialogFragment {

  private static final String TAG = "AlertDialogFragment";

  public static final String DIALOG_TAG = AlertDialogFragment.class.getName() + ".A";

  public AlertDialogFragment() {}

  private AlertDialog mAlertDialog;

  public AlertDialog getDialog() {
    return mAlertDialog;
  }

  private AlertDialogFragment(AlertDialog alertDialog) {
    mAlertDialog = alertDialog;
  }

  private BaseActivity mA;

  public void onAttach(Context context) {
    super.onAttach(context);
    mA = (BaseActivity) getActivity();
  }

  public Dialog onCreateDialog(Bundle savedInstanceState) {
    if (mAlertDialog == null) {
      mAlertDialog = mA.createDialog(requireArguments().getString(DIALOG_TAG), this);
      if (mAlertDialog == null) {
        dismissAllowingStateLoss();
        return new Builder(mA).create();
      }
    }
    UiUtils.onCreateDialog(mAlertDialog, mA);
    return mAlertDialog;
  }

  public void onSaveInstanceState(Bundle outState) {}

  private OnDismissListener mDismissListener;

  public AlertDialogFragment setOnDismissListener(OnDismissListener dismissListener) {
    mDismissListener = dismissListener;
    return this;
  }

  public void onDismiss(DialogInterface dialog) {
    super.onDismiss(dialog);
    if (mDismissListener != null) {
      mDismissListener.onDismiss(dialog);
    }
  }

  public static AlertDialogFragment create(AlertDialog alertDialog, String tag) {
    AlertDialogFragment frag = new AlertDialogFragment(alertDialog);
    Bundle args = new Bundle();
    args.putString(DIALOG_TAG, tag);
    frag.setArguments(args);

    return frag;
  }

  public static AlertDialogFragment show(
      FragmentActivity activity, AlertDialog alertDialog, String tag) {
    return create(alertDialog, tag).show(activity);
  }

  public AlertDialogFragment show(FragmentActivity activity) {
    synchronized (AlertDialogFragment.class) {
      String tag = requireArguments().getString(DIALOG_TAG);
      FragmentManager manager = activity.getSupportFragmentManager();

      Fragment fragment = manager.findFragmentByTag(tag);
      if (fragment != null) {
        manager.beginTransaction().remove(fragment).commitNowAllowingStateLoss();
      }

      try {
        if (activity.getLifecycle().getCurrentState().isAtLeast(State.INITIALIZED)
            && !activity.isChangingConfigurations()) {
          showNow(manager, tag);
        }
      } catch (IllegalStateException e) {
        MyLog.w(TAG, "show", e.toString());
      }

      return this;
    }
  }
}
