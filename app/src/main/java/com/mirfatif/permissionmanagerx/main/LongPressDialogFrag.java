package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.parser.PackageParser.PKG_PARSER;
import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;
import static com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler.DAEMON_HANDLER;

import android.app.Dialog;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.ActivityMainPkgDialogBinding;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.ui.base.BottomSheetDialogFrag;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;

public class LongPressDialogFrag extends BottomSheetDialogFrag {

  private static final String TAG = "LongPressDialogFrag";

  private final Package mPkg;

  LongPressDialogFrag(Package pkg) {
    mPkg = pkg;
  }

  public LongPressDialogFrag() {
    mPkg = null;
  }

  @Nullable
  @Override
  public View onCreateView(
      @NonNull LayoutInflater inflater,
      @Nullable ViewGroup container,
      @Nullable Bundle savedInstanceState) {

    if (mPkg == null) {
      return null;
    }

    ActivityMainPkgDialogBinding b =
        ActivityMainPkgDialogBinding.inflate(inflater, container, container != null);

    boolean canBeExcluded = SETTINGS.canBeExcluded(mPkg);
    boolean canBeDisabled =
        mPkg.isChangeable() && !mPkg.getName().equals(App.getContext().getPackageName());

    if (canBeExcluded) {
      b.excludePkg.setVisibility(View.VISIBLE);
      b.excludePkg.setOnClickListener(
          v -> {
            dismissAllowingStateLoss();
            Utils.runInBg(
                () -> {
                  SETTINGS.addPkgToExcludedApps(mPkg.getName());
                  PKG_PARSER.removePackage(mPkg);
                });
          });
    }

    if (canBeDisabled) {
      b.disablePkg.setVisibility(View.VISIBLE);
      b.disablePkg.setText(mPkg.isEnabled() ? R.string.disable_app : R.string.enable_app);
      b.disablePkg.setOnClickListener(
          v -> {
            dismissAllowingStateLoss();
            setPackageEnabledState();
          });
    }

    b.openPkgInfo.setOnClickListener(
        v -> {
          dismissAllowingStateLoss();
          openAppInfo();
        });

    b.findPkgProc.setOnClickListener(
        v -> {
          dismissAllowingStateLoss();
          openAppProc();
        });

    return b.getRoot();
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
    if (mPkg == null) {
      dismissAllowingStateLoss();
    }
    return super.onCreateDialog(savedInstanceState);
  }

  void setPackageEnabledState() {
    if (mPkg == null) {
      return;
    }
    if (!SETTINGS.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": setPackageEnabledState");
      ((MainActivity) mA).restartPrivDaemon(true, true);
      return;
    }

    boolean enabled = mPkg.isEnabled();

    String warn = null;
    if (enabled && SETTINGS.getBoolPref(R.string.pref_main_warn_dang_change_enc_key)) {
      if (mPkg.isFrameworkApp()) {
        warn = getString(R.string.disable_pkg_warning, getString(R.string.framework));
      } else if (mPkg.isSystemApp()) {
        warn = getString(R.string.disable_pkg_warning, getString(R.string.system));
      }
    }

    if (warn == null) {
      Utils.runInBg(() -> setPackageEnabledState(mPkg, enabled));
      return;
    }

    AlertDialog dialog =
        new Builder(mA)
            .setPositiveButton(
                R.string.yes, (d, which) -> Utils.runInBg(() -> setPackageEnabledState(mPkg, true)))
            .setNegativeButton(R.string.no, null)
            .setNeutralButton(
                R.string.do_not_remind,
                (d, which) -> {
                  SETTINGS.savePref(R.string.pref_main_warn_dang_change_enc_key, false);
                  Utils.runInBg(() -> setPackageEnabledState(mPkg, true));
                })
            .setTitle(R.string.warning)
            .setMessage(Utils.breakParas(warn))
            .create();
    AlertDialogFragment.show(mA, dialog, "PKG_DISABLE_WARNING");
  }

  private void setPackageEnabledState(Package pkg, boolean enabled) {
    String command = pkg.getName() + " " + Utils.getUserId(pkg.getUid());
    if (enabled) {
      command = Commands.DISABLE_PACKAGE + " " + command;
    } else {
      command = Commands.ENABLE_PACKAGE + " " + command;
    }

    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "setPkgEnabledState: sending command: " + command);
    }
    DAEMON_HANDLER.sendRequest(command);
    PKG_PARSER.updatePackage(pkg);
  }

  private void openAppInfo() {
    if (mPkg == null) {
      return;
    }
    int pkgUserId = Utils.getUserId(mPkg.getUid());
    if (Utils.getUserId() == pkgUserId) {
      startActivity(
          new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
              .setData(Uri.parse("package:" + mPkg.getName())));
    } else if (SETTINGS.isPrivDaemonAlive()) {
      String cmd = Commands.OPEN_APP_INFO + " " + mPkg.getName() + " " + pkgUserId;

      if (SETTINGS.isDebug()) {
        Util.debugLog(TAG, "openAppInfo: sending command: " + cmd);
      }

      Utils.runInBg(() -> DAEMON_HANDLER.sendRequest(cmd));
    } else {
      Utils.logDaemonDead(TAG + ": openAppInfo");
      ((MainActivity) mA).restartPrivDaemon(true, true);
    }
  }

  private static final String WRUN_ACTION_SEARCH_PKG = "com.mirfatif.wrun.action.SEARCH_PKG";
  private static final String WRUN_EXTRA_PKG_NAME = "com.mirfatif.wrun.extra.PKG_NAME";
  private static final String WRUN_EXTRA_PKG_UID = "com.mirfatif.wrun.extra.PKG_UID";

  private void openAppProc() {
    if (mPkg == null) {
      return;
    }
    Intent intent = new Intent(WRUN_ACTION_SEARCH_PKG);
    intent
        .putExtra(WRUN_EXTRA_PKG_NAME, mPkg.getName())
        .putExtra(WRUN_EXTRA_PKG_UID, mPkg.getUid());
    try {
      startActivity(intent);
    } catch (ActivityNotFoundException ignored) {
      Snackbar sb =
          Snackbar.make(
              ((MainActivity) mA).getRootView().recyclerView, R.string.wrun_not_installed, 10000);
      sb.setTextColor(mA.getColor(R.color.dynamic_text_color));
      sb.getView().setBackgroundColor(mA.getColor(R.color.dynamicBackground));
      sb.setAction(R.string.install, v -> Utils.openWebUrl(mA, getString(R.string.wrun_url)));
      sb.show();
    }
  }
}
