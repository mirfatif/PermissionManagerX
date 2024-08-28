package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.main.MainActivity.ACTION_SHOW_DRAWER;
import static com.mirfatif.permissionmanagerx.main.MainActivity.TAG_GRANT_ROOT_OR_ADB;
import static com.mirfatif.permissionmanagerx.main.MainActivity.TAG_PRIVS_REQ_FOR_DAEMON;
import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import androidx.appcompat.app.AlertDialog;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.DaemonStarter;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.permissionmanagerx.util.bg.LiveSchedTask;
import java.util.concurrent.TimeUnit;

public class DaemonStartProg {

  private static final String TAG = "DaemonStartProg";

  private final MainActivity mA;

  DaemonStartProg(MainActivity activity) {
    mA = activity;
  }

  void onCreated(String intentAction) {
    DaemonStarter.INS.getLiveProg().observe(mA.mA, this::setDaemonStartProg);
    DaemonStarter.INS.getLiveStartResult().observe(mA.mA, this::handleStartDaemonResult);

    if (MySettings.INS.getShowUnsupportedSdkWarning()) {
      AlertDialog dialog =
          new AlertDialog.Builder(mA.mA)
              .setTitle(R.string.unsupported_sdk_warning_title)
              .setMessage(
                  getString(R.string.unsupported_sdk_warning_message, Utils.getAndroidVersionInt()))
              .setPositiveButton(
                  R.string.unsupported_sdk_warning_button,
                  (d, w) -> MySettings.INS.onUnsupportedSdkWarningShown())
              .create();

      AlertDialogFragment frag = AlertDialogFragment.create(dialog, "UNSUPPORTED_SDK_WARNING");
      frag.setOnDismissListener(d -> startDaemon(intentAction));

      LiveSchedTask.schedule(
          mA.mA,
          () -> frag.show(mA.mA),
          500,
          TimeUnit.MILLISECONDS,
          true,
          TAG + "-SdkWarningDialog");
    } else {
      startDaemon(intentAction);
    }
  }

  private void startDaemon(String intentAction) {
    DaemonStarter.INS.startPrivDaemon(false, true, true, !ACTION_SHOW_DRAWER.equals(intentAction));
  }

  private void setDaemonStartProg(String prog) {
    if (prog != null) {
      mA.mB.bigProgText.setText(prog);
      mA.setBigProgVisible(true);
    } else {
      mA.setBigProgVisible(false);
    }
  }

  private void handleStartDaemonResult(DaemonStarter.DaemonStartResult res) {
    if (res.daemonStarted == DaemonStarter.DaemonStartStatus.NO_PRIVS) {

      if (res.showNoPrivsDialog) {
        if (!res.isFirstRun) {
          AlertDialogFragment.show(mA.mA, null, TAG_GRANT_ROOT_OR_ADB);
        } else if (MySettings.INS.shouldRemindMissingPrivileges()) {
          LiveSchedTask.schedule(
              mA.mA,
              () -> AlertDialogFragment.show(mA.mA, null, TAG_PRIVS_REQ_FOR_DAEMON),
              1,
              TimeUnit.SECONDS,
              true,
              TAG + "-NoPrivsDialog");
        }
      }
    }

    if (res.isFirstRun) {
      mA.setLiveDataObservers();
    } else if (res.wasAlive || res.daemonStarted == DaemonStarter.DaemonStartStatus.STARTED) {
      PackageParser.INS.updatePkgList();
    } else {
      mA.setBigProgVisible(false);
    }

    if (res.daemonStarted == DaemonStarter.DaemonStartStatus.STARTED) {
      mA.mActFlavor.onPrivDaemonStarted();
    } else if (res.daemonStarted == DaemonStarter.DaemonStartStatus.FAILED) {
      mA.showSnackBar(getString(R.string.daemon_failed_toast), 10);
    }
  }
}
