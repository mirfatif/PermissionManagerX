package com.mirfatif.permissionmanagerx.privs;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.os.SystemClock;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.util.bg.LiveEvent;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.bg.BgRunner;
import com.mirfatif.privtasks.util.bg.ThreadUtils;

public enum DaemonStarter {
  INS;

  private static final String TAG = "DaemonStarter";

  public void startPrivDaemon(
      boolean restart, boolean isFirstRun, boolean preferRoot, boolean showNoPrivsDialog) {
    if (ThreadUtils.isMainThread()) {
      BgRunner.execute(() -> startDaemonLocked(restart, isFirstRun, preferRoot, showNoPrivsDialog));
    } else {
      startDaemonLocked(restart, isFirstRun, preferRoot, showNoPrivsDialog);
    }
  }

  private synchronized void startDaemonLocked(
      boolean restart, boolean isFirstRun, boolean preferRoot, boolean showNoPrivsDialog) {
    boolean wasAlive = DaemonHandler.INS.isDaemonAlive();

    mDaemonStartResult.postValue(
        new DaemonStartResult(
            connectOrStartDaemon(restart, wasAlive, preferRoot),
            showNoPrivsDialog,
            isFirstRun,
            wasAlive));
  }

  public synchronized int startPrivDaemon(boolean restart, boolean preferRoot) {
    return connectOrStartDaemon(restart, DaemonHandler.INS.isDaemonAlive(), preferRoot);
  }

  private synchronized int connectOrStartDaemon(
      boolean restart, boolean wasAlive, boolean preferRoot) {
    if (wasAlive && !restart) {
      if (PackageParser.INS.getPkgList().isEmpty()) {
        setProgress(R.string.prog_msg_checking_privs);
      }
      return DaemonStartStatus.STARTED;
    }

    long waitTill = System.currentTimeMillis();

    if (wasAlive) {
      setProgress(R.string.prog_msg_stopping_daemon);
      if (DaemonHandler.INS.stopDaemon()) {
        waitTill += 1000;
      }
    } else {
      setProgress(R.string.prog_msg_connecting_to_daemon);
      if (DaemonHandler.INS.isDaemonAlive(false, true)) {
        return DaemonStartStatus.STARTED;
      }
    }

    boolean hasPrivs = false;

    if (MySettings.INS.isRootEnabled() || MySettings.INS.isAdbEnabled()) {
      if (preferRoot) {
        setProgress(R.string.prog_msg_checking_root_access);
        hasPrivs = NativeDaemon.getRoot();
      }

      if (!hasPrivs) {
        setProgress(R.string.prog_msg_checking_adb_access);
        hasPrivs = NativeDaemon.getAdb();
      }

      if (!hasPrivs && !preferRoot) {
        setProgress(R.string.prog_msg_checking_root_access);
        hasPrivs = NativeDaemon.getRoot();
      }
    }

    if (!hasPrivs) {
      MyLog.w(TAG, "connectOrStartDaemon", "Root / ABD access unavailable");
      if (!PackageParser.INS.getPkgList().isEmpty()) {
        mProgress.postValue(null);
      }
      return DaemonStartStatus.NO_PRIVS;
    }

    setProgress(R.string.prog_msg_starting_daemon);

    long sleep = waitTill - System.currentTimeMillis();
    if (sleep > 0) {
      SystemClock.sleep(sleep);
    }

    return DaemonHandler.INS.startDaemon(preferRoot)
        ? DaemonStartStatus.STARTED
        : DaemonStartStatus.FAILED;
  }

  public void switchToRootOrAdbDaemon(boolean preferRoot) {
    if (Boolean.valueOf(preferRoot).equals(DaemonHandler.INS.isPreferRoot())) {
      MyLog.i(
          TAG,
          "switchToRootOrAdbDaemon",
          (preferRoot ? "root" : "adb") + " daemon already running");
      return;
    }
    startPrivDaemon(true, false, preferRoot, true);
  }

  public void stopDaemon(boolean preferRoot) {
    boolean restart = true;

    if (Boolean.valueOf(preferRoot).equals(DaemonHandler.INS.isPreferRoot())) {
      MyLog.i(TAG, "stopDaemon", (preferRoot ? "root" : "adb") + " daemon already running");
      restart = false;
    }

    startPrivDaemon(restart, false, false, false);
  }

  private final LiveEvent<String> mProgress = new LiveEvent<>(true);
  private final LiveEvent<DaemonStartResult> mDaemonStartResult = new LiveEvent<>(true);

  public LiveEvent<String> getLiveProg() {
    return mProgress;
  }

  public LiveEvent<DaemonStartResult> getLiveStartResult() {
    return mDaemonStartResult;
  }

  private void setProgress(int msg) {
    mProgress.postValue(getString(msg));
  }

  public static class DaemonStartResult {
    public final int daemonStarted;
    public final boolean showNoPrivsDialog;
    public final boolean isFirstRun;
    public final boolean wasAlive;

    private DaemonStartResult(
        int started, boolean showNoPrivsDialog, boolean isFirstRun, boolean wasAlive) {
      this.daemonStarted = started;
      this.showNoPrivsDialog = showNoPrivsDialog;
      this.isFirstRun = isFirstRun;
      this.wasAlive = wasAlive;
    }
  }

  public @interface DaemonStartStatus {
    int NO_PRIVS = 0;
    int STARTED = 1;
    int FAILED = 2;
  }
}
