package com.mirfatif.permissionmanagerx.privs;

import static com.mirfatif.permissionmanagerx.BuildConfig.APPLICATION_ID;
import static com.mirfatif.permissionmanagerx.BuildConfig.APP_ID;
import static com.mirfatif.permissionmanagerx.privs.DaemonHandler.UID_ROOT;
import static java.lang.System.currentTimeMillis;

import android.os.SystemClock;
import com.mirfatif.err.AdbException;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.StdErrLogServer;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.Util;
import io.github.muntashirakon.adb.AdbStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.Inet4Address;
import java.net.Socket;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

public enum NativeDaemon {
  INS_R(false),
  INS_A(true);

  private static final String STATIC_TAG = "NativeDaemon";

  private final boolean mAdb;
  private final String DAEMON_COMMAND;
  private final String TAG;

  NativeDaemon(boolean isAdb) {
    mAdb = isAdb;
    String libDir = ApiUtils.getMyAppInfo().nativeLibraryDir;
    String bin = new File(libDir, "libpmxe.so").getAbsolutePath();

    String suffix = APPLICATION_ID.replace(APP_ID, "");
    suffix = suffix.replaceFirst("^\\.", "");
    if (suffix.length() != 0) {
      suffix += "-";
    }

    DAEMON_COMMAND = "exec " + bin + " -D " + suffix + (isAdb ? "adb" : "root");
    TAG = isAdb ? "AdbNativeDaemon" : "RootNativeDaemon";
  }

  private final Object LOCK = new Object();

  private void connectToCheckAlive() {
    synchronized (LOCK) {
      if (MySettings.INS.shouldExtractFiles()) {
        savePort(0);
        return;
      }

      int port = mAdb ? MySettings.INS.getAdbDaemonPort() : MySettings.INS.getRootDaemonPort();
      if (port <= 0) {
        return;
      }

      connectToDaemon(port, false);

      if (!isAlive(false)) {
        savePort(0);
      }
    }
  }

  private void connectToDaemon(int port, boolean printException) {
    Socket socket = null;
    try {
      try {
        socket = new Socket(Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}), port);
      } catch (IOException e) {
        if (printException) {
          throw e;
        } else {
          MyLog.e(TAG, "connectToDaemon", e.toString());
          return;
        }
      }

      socket.setTcpNoDelay(true);

      OutputStream os = socket.getOutputStream();
      PrintWriter writer = new PrintWriter(os, true);

      try (StdErrLogServer server = new StdErrLogServer(TAG, () -> daemonStopped(true))) {
        writer.println(CMD_HELLO + server.getLocalPort());
        BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        socket.setSoTimeout(5000);
        String resp = reader.readLine();
        socket.setSoTimeout(0);

        String[] respSplit;
        int uid;
        if (resp == null
            || !resp.startsWith(RESP_UID)
            || ((respSplit = resp.split(" ")).length) != 2
            || (uid = Integer.parseInt(respSplit[1])) < 0) {
          return;
        }

        server.waitForConn();

        mState = new State(uid, reader, writer, os);
      }
    } catch (IOException | NumberFormatException | InterruptedException | ExecutionException e) {
      MyLog.e(TAG, "connectToDaemon", e);
    } finally {
      if (!isAlive(false) && socket != null) {
        try {
          socket.close();
        } catch (IOException ignored) {
        }
      }
    }
  }

  private boolean startRootDaemon() {
    synchronized (LOCK) {
      if (isAlive(false)) {
        MyLog.w(TAG, "startRootDaemon", "Daemon already running");
        return true;
      }

      String su = MySettings.INS.getSuExePath();
      Process proc = Util.runProc(TAG, "startRootDaemon", false, su);
      if (proc == null) {

        MySettings.INS.setRootEnabled(false);
        return false;
      }

      MyLog.i(TAG, "startRootDaemon", "Starting root native daemon");

      PrintWriter printer = new PrintWriter(proc.getOutputStream());
      printer.println(DAEMON_COMMAND);
      printer.close();

      waitForPort(new BufferedReader(new InputStreamReader(proc.getInputStream())));

      boolean rootEnabled = isAlive(false);
      MySettings.INS.setRootEnabled(rootEnabled);
      return rootEnabled;
    }
  }

  private boolean startAdbDaemon(
      boolean retryConnect, boolean printException, String host, int port) {
    synchronized (LOCK) {
      if (isAlive(false)) {
        MyLog.w(TAG, "startAdbDaemon", "Daemon already running");
        return true;
      }

      boolean err = false;
      Exception exception = null;

      long ts = currentTimeMillis() + 5000;
      for (int i = 0; i <= 10; i++) {
        err = false;
        exception = null;

        try (AdbConnManager connMgr = new AdbConnManager()) {
          connMgr.setTimeout(10, TimeUnit.SECONDS);
          boolean res;
          if (i == 0) {
            res = connMgr.autoConnect(App.getCxt(), 2500);
          } else if (host == null || port <= 0) {
            err = true;
            break;
          } else {
            res = connMgr.connect(host, port);
          }
          if (res) {
            try (AdbStream adbStream = connMgr.openStream("shell:" + DAEMON_COMMAND)) {
              waitForPort(new BufferedReader(new InputStreamReader(adbStream.openInputStream())));
            } catch (IOException | InterruptedException e) {
              MyLog.e(TAG, "startAdbDaemon", "ADB stream failed", e);
            }
            break;
          } else {
            err = true;
          }

        } catch (AdbException | InterruptedException | IOException e) {
          exception = e;
        }

        if (i == 0) {
          continue;
        }

        if (!retryConnect || (ts - currentTimeMillis() <= 0)) {
          break;
        }

        if (i < 10) {
          SystemClock.sleep(500);
          MyLog.i(TAG, "startAdbDaemon", "Retrying " + (i + 1));
        }
      }

      if (exception != null) {
        if (printException) {
          MyLog.e(TAG, "startAdbDaemon", "Adb connection failed", exception);
        } else {
          MyLog.e(TAG, "startAdbDaemon", "Adb connection failed: " + exception);
        }
      } else if (err) {
        MyLog.e(TAG, "startAdbDaemon", "Adb connection failed");
      }

      boolean adbEnabled = isAlive(false);
      MySettings.INS.setAdbEnabled(adbEnabled);
      return adbEnabled;
    }
  }

  private void waitForPort(BufferedReader reader) {
    try (reader) {
      String resp = reader.readLine();

      String[] respSplit;
      int port;
      if (resp == null
          || !resp.startsWith(RESP_PORT)
          || ((respSplit = resp.split(" ")).length) != 2
          || (port = Integer.parseInt(respSplit[1])) <= 0) {
        return;
      }

      connectToDaemon(port, true);
      if (isAlive(false)) {
        savePort(port);
        setExitOnAppDeathUnlocked();
      }
    } catch (IOException e) {
      MyLog.e(TAG, "waitForPort", e);
    }
  }

  public void stopDaemon() {
    synchronized (LOCK) {
      if (isAlive(true)) {
        stopDaemonUnlocked();
      }
    }
  }

  private void stopDaemonUnlocked() {
    sendCmd(CMD_EXIT, null);
    daemonStopped(false);
  }

  private void daemonStopped(boolean holdLock) {
    if (mState != null) {
      if (holdLock) {
        synchronized (LOCK) {
          mState = null;
          savePort(0);
        }
      } else {
        mState = null;
        savePort(0);
      }
    }
  }

  public static final int NATIVE_DAEMON_RESTART_WAIT = 5000;

  private void restartDaemon() {
    daemonStopped(true);
    MyLog.e(TAG, "restartDaemon", (mAdb ? "ADB" : "Root") + " daemon died");
    UiUtils.showToast(R.string.native_daemon_died_toast);

    SystemClock.sleep(NATIVE_DAEMON_RESTART_WAIT);

    MyLog.i(TAG, "restartDaemon", "Restarting native daemon");
    if (mAdb
        ? startAdbDaemon(false, true, MySettings.INS.getAdbHost(), MySettings.INS.getAdbPort())
        : startRootDaemon()) {
      UiUtils.showToast(R.string.native_daemon_restarted_toast);
    }
  }

  private void savePort(int port) {
    if (mAdb) {
      MySettings.INS.saveAdbDaemonPort(port);
    } else {
      MySettings.INS.saveRootDaemonPort(port);
    }
  }

  public void setExitOnAppDeath() {
    synchronized (LOCK) {
      if (isAlive(true, true)) {
        setExitOnAppDeathUnlocked();
      }
    }
  }

  public void setExitOnAppDeathUnlocked() {
    sendCmd(CMD_AUTO_EXIT + " " + MySettings.INS.shouldDaemonExitOnAppDeath(), null);
  }

  private boolean isAlive(boolean tryConnect) {
    return isAlive(tryConnect, false);
  }

  private boolean isAlive(boolean tryConnect, boolean forAutoExit) {
    if (mState == null
        && tryConnect
        && (forAutoExit || !MySettings.INS.shouldDaemonExitOnAppDeath())) {
      connectToCheckAlive();
    }
    return mState != null;
  }

  public Boolean isRoot() {
    synchronized (LOCK) {
      if (mState == null) {
        return null;
      }
      return mState.mUid == UID_ROOT;
    }
  }

  private static final String CMD_HELLO = "hello ";
  private static final String CMD_EXIT = "exit";
  private static final String CMD_AUTO_EXIT = "auto_exit";
  private static final String CMD_RUN = "run ";
  private static final String CMD_RUN_BG = "run_bg ";
  private static final String CMD_RUN_DMN = "run_dmn ";
  private static final String CMD_FILE_EXIST = "exist ";
  private static final String CMD_SET_PERM = "perm ";

  private static final String RESP_PORT = "PORT:";
  private static final String RESP_UID = "UID:";
  private static final String RESP_EXIST = "EXIST";
  private static final String RESP_SAVED = "SAVED";

  private static final String SELABEL_PARENT = "PARENT";

  public boolean fileExists(String path) {
    return sendCommandLocked(CMD_FILE_EXIST + path, RESP_EXIST);
  }

  public void setPerm(String file, int uid, int gid, String mode, String seLabel) {
    if (seLabel == null) {
      seLabel = SELABEL_PARENT;
    }
    sendCommand(CMD_SET_PERM + uid + " " + gid + " " + mode + " " + seLabel + " " + file);
  }

  public void run(String cmd, boolean background) {
    sendCommand((background ? CMD_RUN_BG : CMD_RUN) + cmd);
  }

  public void runDaemon(
      String cp, int guid, String seLabel, String niceName, String cls, String... args) {
    StringBuilder argStr = new StringBuilder();
    for (String s : args) {
      argStr.append(" ").append(s);
    }
    sendCommand(
        CMD_RUN_DMN + cp + " " + guid + " " + seLabel + " " + niceName + " " + cls + argStr);
  }

  public boolean extractDex(String path) {
    synchronized (LOCK) {
      if (!isAlive(false)) {
        return false;
      }

      if (!NativeDaemonFlavor.extractDex(path, this, mState.mOutStream)) {
        return false;
      }
      return matchResponse(RESP_SAVED);
    }
  }

  private void sendCommand(String cmd) {
    sendCommandLocked(cmd, null);
  }

  private boolean sendCommandLocked(String cmd, String match) {
    synchronized (LOCK) {
      if (!isAlive(false)) {
        return false;
      }
      return sendCmd(cmd, match);
    }
  }

  boolean sendCmd(String cmd, String match) {
    if (match != null && !cleanResponseReader()) {
      return false;
    }

    if (BuildConfig.DEBUG) {
      MyLog.d(TAG, "sendCmd", "Sending command: " + cmd);
    }

    mState.mCmdWriter.println(cmd);

    if (match == null) {
      return true;
    }
    return matchResponse(match);
  }

  private boolean cleanResponseReader() {
    try {
      StringBuilder str = new StringBuilder();
      while (mState.mResponseReader.ready()) {
        str.append((char) mState.mResponseReader.read());
      }
      if (str.length() != 0) {
        for (String line : str.toString().split("\n")) {
          MyLog.i(TAG, "cleanResponseReader", line);
        }
      }
      return true;
    } catch (IOException e) {
      MyLog.e(TAG, "cleanResponseReader", e);
      restartDaemon();
      return false;
    }
  }

  private boolean matchResponse(String match) {
    try {
      String resp = mState.mResponseReader.readLine();
      if (BuildConfig.DEBUG) {
        MyLog.d(TAG, "matchResponse", "Response: " + resp);
      }
      return match.equals(resp);
    } catch (IOException e) {
      MyLog.e(TAG, "matchResponse", e);
      restartDaemon();
      return false;
    }
  }

  private State mState;

  private static class State {

    private final int mUid;
    private final BufferedReader mResponseReader;
    private final PrintWriter mCmdWriter;
    private final OutputStream mOutStream;

    private State(int uid, BufferedReader reader, PrintWriter writer, OutputStream os) {
      mUid = uid;
      mResponseReader = reader;
      mCmdWriter = writer;
      mOutStream = os;
    }
  }

  public static boolean hasRoot(boolean tryConnect) {
    return INS_R.isAlive(tryConnect);
  }

  public static boolean getRoot() {
    return getRoot(true);
  }

  public static boolean getRoot(boolean ifEnabledOnly) {
    return hasRoot(true)
        || ((!ifEnabledOnly || MySettings.INS.isRootEnabled()) && INS_R.startRootDaemon());
  }

  public static boolean hasAdb(boolean tryConnect) {
    return INS_A.isAlive(tryConnect);
  }

  public static boolean getAdb() {
    return getAdb(true, false, false);
  }

  private static boolean getAdb(
      boolean ifEnabledOnly, boolean retryConnect, boolean printException) {
    return getAdb(
        ifEnabledOnly,
        retryConnect,
        printException,
        MySettings.INS.getAdbHost(),
        MySettings.INS.getAdbPort());
  }

  public static boolean getAdb(
      boolean ifEnabledOnly, boolean retryConnect, boolean printException, String host, int port) {
    return hasAdb(true)
        || ((!ifEnabledOnly || MySettings.INS.isAdbEnabled())
            && INS_A.startAdbDaemon(retryConnect, printException, host, port));
  }

  public static boolean forceGetAdb(boolean retryConnect) {
    if (getAdb(false, retryConnect, false)) {
      return true;
    }

    if (!getRoot()) {
      return false;
    }

    MyLog.i(STATIC_TAG, "forceGetAdb", "Sending ADB switch commands");
    INS_R.run("settings put global adb_enabled 0", false);
    INS_R.run("stop adbd", false);
    INS_R.run("setprop service.adb.tcp.port " + MySettings.INS.getAdbPort(), false);
    SystemClock.sleep(2000);
    INS_R.run("settings put global adb_enabled 1", false);
    INS_R.run("start adbd", false);

    return getAdb(false, true, true);
  }
}
