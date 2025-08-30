package com.mirfatif.permissionmanagerx.privs;

import static com.mirfatif.permissionmanagerx.BuildConfig.APPLICATION_ID;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.INS_A;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.INS_R;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.NATIVE_DAEMON_RESTART_WAIT;
import static java.lang.System.currentTimeMillis;

import android.content.Intent;
import android.os.Bundle;
import android.os.IBinder;
import android.os.RemoteException;
import android.os.SystemClock;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.svc.LogcatSvc;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.AppLifecycle;
import com.mirfatif.permissionmanagerx.util.LogUtils;
import com.mirfatif.permissionmanagerx.util.StdErrLogServer;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.privtasks.Constants;
import com.mirfatif.privtasks.PrivTasksError;
import com.mirfatif.privtasks.bind.DaemonState;
import com.mirfatif.privtasks.bind.IPrivTasksCallback;
import com.mirfatif.privtasks.iface.IPrivTasks;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.bg.NotifyWaiter;
import com.mirfatif.privtasks.util.bg.RateLimiter;
import com.mirfatif.privtasks.util.bg.ThreadUtils;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.Inet4Address;
import java.net.Socket;
import java.util.NoSuchElementException;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public enum DaemonHandler {
  INS;

  private static final String TAG = "DaemonHandler";

  private Boolean mPreferRoot;

  private boolean startDaemon() {
    return startDaemon(mPreferRoot);
  }

  private final AtomicBoolean START_DAEMON_LOCK = new AtomicBoolean();

  public boolean startDaemon(Boolean preferRoot) {
    synchronized (START_DAEMON_LOCK) {
      if (isDaemonAlive()) {
        MyLog.w(TAG, "startDaemon", "Daemon already running");
        return true;
      } else {
        return startDaemonInternal(preferRoot != Boolean.FALSE);
      }
    }
  }

  private boolean startDaemonInternal(boolean preferRoot) {
    boolean hasRoot = false, hasAdb = false;

    if (!preferRoot) {
      hasAdb = NativeDaemon.getAdb();
    }

    if (!hasAdb) {
      hasRoot = NativeDaemon.getRoot();
    }

    if (preferRoot && !hasRoot) {
      hasAdb = NativeDaemon.getAdb();
    }

    if (!hasRoot && !hasAdb) {
      MyLog.e(TAG, "startDaemon", "Cannot start privileged daemon without root or ADB shell");
      return false;
    }

    mPreferRoot = (preferRoot || !hasAdb) && hasRoot;

    var daemon = mPreferRoot ? INS_R : INS_A;
    daemon.runDaemon(CODE_WORD);

    if (waitForHello()) {
      MySettings.INS.setDaemonStartTs(currentTimeMillis());
      return true;
    } else {
      return false;
    }
  }

  private static final String CODE_WORD = UUID.randomUUID().toString();

  private boolean connectToDaemon() {
    synchronized (START_DAEMON_LOCK) {
      if (isDaemonAlive()) {
        return true;
      }

      if (MySettings.INS.shouldRestartDaemon()) {
        savePort(0);
        return false;
      }

      if (MySettings.INS.shouldDaemonExitOnAppDeath()) {
        return false;
      }

      boolean res = false;

      int port = MySettings.INS.getDaemonPort();
      if (port == 0) {
        return false;
      }

      try (Socket socket = new Socket(Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}), port)) {
        socket.setTcpNoDelay(true);
        PrintWriter writer = new PrintWriter(socket.getOutputStream());
        writer.println(Constants.CMD_CODE_WORD + ": " + CODE_WORD);
        writer.close();
        res = true;
      } catch (IOException ignored) {
      }

      if (res) {
        res = waitForHello();
      } else {
        savePort(0);
      }

      return res;
    }
  }

  private void savePort(int port) {
    MySettings.INS.saveDaemonPort(port);
  }

  private final AtomicReference<DaemonState> DAEMON_LIVE_STATE = new AtomicReference<>();

  public void onBinderReceived(Intent intent) {
    ThreadUtils.assertNotMainThread();

    String codeWord = intent.getStringExtra(Constants.EXTRA_CODE_WORD);
    Bundle bundle;
    IBinder iBinder;
    if (!CODE_WORD.equals(codeWord)
        || (bundle = intent.getBundleExtra(Constants.EXTRA_BINDER)) == null
        || (iBinder = bundle.getBinder(Constants.EXTRA_BINDER)) == null) {
      onGreetingsEnd();
      MyLog.e(TAG, "onBinderReceived", "Bad intent received");
      return;
    }

    IPrivTasks privTasks = IPrivTasks.Stub.asInterface(iBinder);

    synchronized (DAEMON_LIVE_STATE) {
      daemonStopped();
    }

    String jniLib =
        new File(ApiUtils.getMyAppInfo().nativeLibraryDir, "libpmxd.so").getAbsolutePath();

    try (StdErrLogServer server = new StdErrLogServer(TAG, null)) {
      privTasks.sendStdErr(server.getLocalPort(), jniLib);
      server.waitForConn();
      privTasks.hello(new IPrivTasksCb(privTasks), LogUtils.createCrashLogFile().getAbsolutePath());
    } catch (RemoteException | IOException | InterruptedException | ExecutionException e) {
      MyLog.e(TAG, "onBinderReceived", e);
      onGreetingsEnd();
    }
  }

  private void onHelloReceived(DaemonState state) {
    try {
      state.deathRecipient = DaemonHandler.this::onDaemonDied;
      state.privTasks.asBinder().linkToDeath(state.deathRecipient, 0);

      DAEMON_LIVE_STATE.set(state);
      savePort(state.port);

      DaemonIface.INS.onDaemonStarted(state.privTasks);
      if (AppLifecycle.isAppInFg()) {
        LogcatSvc.sendDaemonCallback();
      }
      DaemonIface.INS.setExitOnAppDeath();
    } catch (RemoteException e) {
      MyLog.e(TAG, "onHelloReceived", e);

      synchronized (DAEMON_LIVE_STATE) {
        daemonStopped();
      }
    }

    if (MySettings.INS.shouldGrantAppPrivs()) {
      try {
        state.privTasks.grantAppPrivileges(APPLICATION_ID, ApiUtils.getMyAppInfo().uid);
      } catch (RemoteException e) {
        MyLog.e(TAG, "onHelloReceived", e);
        UiUtils.showToast(R.string.granting_permissions_failed_toast);
      }
    }

    onGreetingsEnd();
  }

  private final NotifyWaiter mHelloWaiter = new NotifyWaiter(5, TimeUnit.SECONDS);

  private boolean waitForHello() {
    mHelloWaiter.waitForNotifyNoThrow();
    return isDaemonAlive();
  }

  private void onGreetingsEnd() {
    mHelloWaiter.notify(true);
  }

  private void daemonStopped() {
    DaemonIface.INS.onDaemonStopped();
    DAEMON_LIVE_STATE.set(null);
    savePort(0);
  }

  private void onDaemonDied() {
    boolean restart = false;
    synchronized (DAEMON_LIVE_STATE) {
      if (isDaemonAlive()) {
        DaemonState state = DAEMON_LIVE_STATE.get();
        MyLog.e(
            TAG,
            "onDaemonDied",
            "Privileged daemon (PID: " + state.pid + ", UID: " + state.uid + ") died");
        restart = true;
      }
      daemonStopped();
    }

    if (!restart) {
      return;
    }

    UiUtils.showToast(R.string.priv_daemon_died_toast);

    SystemClock.sleep(NATIVE_DAEMON_RESTART_WAIT * 2);

    MyLog.i(TAG, "onDaemonDied", "Restarting privileged daemon");
    if (startDaemon()) {
      UiUtils.showToast(R.string.priv_daemon_restarted_toast);
    }
  }

  public boolean stopDaemon() {
    ThreadUtils.assertNotMainThread();

    synchronized (DAEMON_LIVE_STATE) {
      boolean alive = isDaemonAlive(false, true);
      DaemonState state = DAEMON_LIVE_STATE.get();
      daemonStopped();

      if (state != null) {
        try {
          state.privTasks.asBinder().unlinkToDeath(state.deathRecipient, 0);
        } catch (NoSuchElementException ignored) {
        }

        try {
          state.privTasks.stopDaemon();
        } catch (RemoteException ignored) {
        }
      }

      return alive;
    }
  }

  private class IPrivTasksCb extends IPrivTasksCallback.Stub {

    private final IPrivTasks mPrivTasks;

    public IPrivTasksCb(IPrivTasks privTasks) {
      mPrivTasks = privTasks;
    }

    public void hello(DaemonState daemonState) {
      daemonState.privTasks = mPrivTasks;
      onHelloReceived(daemonState);
    }

    public void showError(int error) {
      DaemonHandler.this.showError(error);
    }

    public void saveLog(String stackTrace) {
      LogUtils.showCrashNotification(stackTrace, true);
    }
  }

  public void showError(int error) {
    switch (error) {
      case PrivTasksError.OP_NUM_INCONSISTENCY,
          PrivTasksError.OP_MODE_INCONSISTENCY,
          PrivTasksError.APP_OPS_IMPL ->
          UiUtils.showToast(R.string.daemon_err_bad_rom_toast);
      default -> MyLog.e(TAG, "", "Bad error code: " + error);
    }
  }

  public boolean isDaemonAlive() {
    return isDaemonAlive(false, false);
  }

  private final RateLimiter mDaemonDeadLogLimiter = new RateLimiter(1, TimeUnit.SECONDS);

  public boolean isDaemonAlive(boolean showToast, boolean tryConnect) {
    boolean res = DAEMON_LIVE_STATE.get() != null;
    if (!res && tryConnect) {
      res = connectToDaemon();
    }

    if (!res) {
      if (showToast) {
        UiUtils.showToast(R.string.daemon_not_running_toast);
      }

      if (MySettings.INS.isDebug() || mDaemonDeadLogLimiter.can(true)) {
        MyLog.w(TAG, null, "Privileged daemon is not running");
      }
    }

    return res;
  }

  public Boolean isPreferRoot() {
    if (!isDaemonAlive()) {
      return null;
    }
    return mPreferRoot;
  }

  public Integer getUid() {
    synchronized (DAEMON_LIVE_STATE) {
      if (!isDaemonAlive()) {
        return null;
      }
      return DAEMON_LIVE_STATE.get().uid;
    }
  }

  public String getContext() {
    synchronized (DAEMON_LIVE_STATE) {
      if (!isDaemonAlive()) {
        return null;
      }
      return DAEMON_LIVE_STATE.get().context;
    }
  }
}
