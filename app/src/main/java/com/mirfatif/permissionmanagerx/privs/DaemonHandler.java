package com.mirfatif.permissionmanagerx.privs;

import static android.os.Build.VERSION.SDK_INT;
import static com.mirfatif.permissionmanagerx.BuildConfig.APPLICATION_ID;
import static com.mirfatif.permissionmanagerx.BuildConfig.APP_ID;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.INS_A;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.INS_R;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.NATIVE_DAEMON_RESTART_WAIT;
import static java.lang.System.currentTimeMillis;

import android.content.Intent;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.os.RemoteException;
import android.os.SystemClock;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.fwk.DaemonRcvSvcM;
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

  public static final int UID_SYSTEM = android.os.Process.SYSTEM_UID;
  public static final int UID_ROOT =
      SDK_INT >= Build.VERSION_CODES.Q ? android.os.Process.ROOT_UID : 0;
  public static final int UID_SHELL =
      SDK_INT >= Build.VERSION_CODES.Q ? android.os.Process.SHELL_UID : 2000;

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
      }

      if (preferRoot == null) {
        preferRoot = true;
      }

      boolean dexInTmpDir = MySettings.INS.loadDexFromTmpDir();
      boolean res = startDaemon(preferRoot, dexInTmpDir);
      if (res) {
        return true;
      }
      res = startDaemon(preferRoot, !dexInTmpDir);
      if (res) {
        UiUtils.showToast(R.string.dex_location_changed_toast);
        MySettings.INS.setLoadDexFromTmpDir(!dexInTmpDir);
      }
      return res;
    }
  }

  private boolean startDaemon(boolean preferRoot, boolean dexInTmpDir) {
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

    boolean extractDex = MySettings.INS.shouldExtractFiles();
    if (extractDex) {
      extractDexFile();
    }

    String dexFilePath = dexInTmpDir ? TMP_DIR_DEX_PATH : SHARED_DIR_DEX_PATH;

    if (!dexExists(dexFilePath)) {
      if (!extractDex) {
        MySettings.INS.setFileExtractionTs(0);
        return startDaemon(preferRoot, dexInTmpDir);
      } else {
        UiUtils.showToast(R.string.files_not_extracted_accessible_toast);
        return false;
      }
    } else if (extractDex) {
      MySettings.INS.setFileExtractionTs(currentTimeMillis());
    }

    String cxt = MySettings.INS.getDaemonContext();
    String vmClass = DAEMON_PACKAGE_NAME + "." + DAEMON_CLASS_NAME;

    (mPreferRoot ? INS_R : INS_A)
        .runDaemon(
            dexFilePath,
            MySettings.INS.getDaemonUid(),
            cxt,
            VM_NAME,
            vmClass,
            APPLICATION_ID,
            DaemonRcvSvcM.class.getName(),
            CODE_WORD);

    return waitForHello();
  }

  private static final String CODE_WORD = UUID.randomUUID().toString();

  private boolean connectToDaemon() {
    synchronized (START_DAEMON_LOCK) {
      if (isDaemonAlive()) {
        return true;
      }

      if (MySettings.INS.shouldExtractFiles()) {
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

    try {
      StdErrLogServer server = new StdErrLogServer(TAG, null);
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
      DaemonIfaceFlavor.INS.onDaemonStarted(state.privTasksFlavor);
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
    DaemonIfaceFlavor.INS.onDaemonStopped();
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
      case PrivTasksError.OP_NUM_INCONSISTENCY:
      case PrivTasksError.OP_MODE_INCONSISTENCY:
      case PrivTasksError.APP_OPS_IMPL:
        UiUtils.showToast(R.string.daemon_err_bad_rom_toast);
        break;
      default:
        MyLog.e(TAG, "", "Bad error code: " + error);
        break;
    }
  }

  private static final String DAEMON_PACKAGE_NAME = "com.mirfatif.privdaemon";
  private static final String DAEMON_CLASS_NAME = "Main";

  private static final String TMP_DIR = "/data/local/tmp/";

  private static final String VM_NAME;

  private static final String SHARED_DIR_DEX_PATH;
  private static final String TMP_DIR_DEX_PATH;

  static {
    VM_NAME = APPLICATION_ID.replace(APP_ID, DAEMON_PACKAGE_NAME + ".pmx");
    String dex = VM_NAME + ".dex";
    String sharedDir = App.getCxt().getExternalFilesDir(null).getAbsolutePath();
    SHARED_DIR_DEX_PATH = new File(sharedDir, dex).getAbsolutePath();
    TMP_DIR_DEX_PATH = new File(TMP_DIR, dex).getAbsolutePath();
  }

  private final Object EXTRACT_DEX_LOCK = new Object();

  private void extractDexFile() {
    synchronized (EXTRACT_DEX_LOCK) {
      NativeDaemon daemon;
      if (NativeDaemon.hasRoot(false)) {
        daemon = INS_R;
      } else if (NativeDaemon.hasAdb(false)) {
        daemon = INS_A;
      } else {
        return;
      }

      for (String file : new String[] {SHARED_DIR_DEX_PATH, TMP_DIR_DEX_PATH}) {
        if (!daemon.extractDex(file)) {
          MyLog.e(TAG, "extractDexFile", "Extracting failed: " + file);
          return;
        }

        if (file.equals(TMP_DIR_DEX_PATH) && Boolean.TRUE.equals(daemon.isRoot())) {
          daemon.setPerm(TMP_DIR_DEX_PATH, UID_SHELL, UID_SHELL, "0644", null);
        }

        MyLog.i(TAG, "extractDexFile", "Extracted successfully: " + file);
      }
    }
  }

  private boolean dexExists(String dexPath) {
    if (NativeDaemon.hasRoot(false)) {
      return INS_R.fileExists(dexPath);
    } else if (NativeDaemon.hasAdb(false)) {
      return INS_A.fileExists(dexPath);
    } else {
      return false;
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

  public boolean makeDaemonAlive() {
    if (!isDaemonAlive(false, true)) {
      if (!NativeDaemon.getRoot() && !NativeDaemon.getAdb()) {
        if (AppLifecycle.isAppVisible()) {
          UiUtils.showToast(R.string.daemon_not_running_toast);
        }
        return false;
      }

      if (!startDaemon()) {
        MyLog.e(TAG, "makeDaemonAlive", "Starting privileged daemon failed");
        if (AppLifecycle.isAppVisible()) {
          UiUtils.showToast(R.string.daemon_failed_toast);
        }
        return false;
      }
    }

    return true;
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

  public boolean isSystemUid() {
    Integer uid = getUid();
    return uid != null && uid == UID_SYSTEM;
  }

  public boolean isRootUid() {
    Integer uid = getUid();
    return uid != null && uid == UID_ROOT;
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
