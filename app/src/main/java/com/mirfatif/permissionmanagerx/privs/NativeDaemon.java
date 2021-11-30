package com.mirfatif.permissionmanagerx.privs;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.os.SystemClock;
import android.util.Log;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.net.Inet4Address;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public enum NativeDaemon {
  ROOT_DAEMON(false),
  ADB_DAEMON(true);

  private final boolean mIsAdb;
  private final String TAG;

  NativeDaemon(boolean isAdb) {
    mIsAdb = isAdb;
    if (isAdb) {
      TAG = "AdbNativeDaemon";
    } else {
      TAG = "RootNativeDaemon";
    }
  }

  public static final String PMX_BIN_PATH;

  static {
    String libDir = App.getContext().getApplicationInfo().nativeLibraryDir;
    PMX_BIN_PATH = new File(libDir, "libpmxe.so").getAbsolutePath();
  }

  private static final String DAEMON_COMMAND = "exec " + PMX_BIN_PATH + " -D";

  private boolean mIsRunning = false;
  private OutputStream mOutStream;
  private BufferedReader mResponseReader;
  private PrintWriter mCmdWriter;

  public boolean isRunning() {
    return isRunning(true);
  }

  public boolean isRunning(boolean showAdbFailedToast) {
    if (!mIsRunning) {
      startDaemon(showAdbFailedToast);
    }
    return mIsRunning;
  }

  private final Object READ_WRITE_LOCK = new Object();

  private synchronized void startDaemon(boolean showAdbFailedToast) {
    if (mIsRunning) {
      Log.w(TAG, "startDaemon: daemon already running");
      return;
    }

    synchronized (READ_WRITE_LOCK) {
      if (mIsAdb) {
        startAdbDaemon(showAdbFailedToast);
        SETTINGS.setAdbConnected(mIsRunning);
      } else {
        startRootDaemon();
        SETTINGS.setRootGranted(mIsRunning);
      }
      updateDrawer();
    }
  }

  private void startRootDaemon() {
    Process suProcess = Utils.runCommand(TAG + ": startRootDaemon", false, Utils.getSu());
    if (suProcess == null) {
      return;
    }

    mOutStream = suProcess.getOutputStream();
    mResponseReader = new BufferedReader(new InputStreamReader(suProcess.getInputStream()));
    mCmdWriter = new PrintWriter(mOutStream, true);

    Log.i(TAG, "startRootDaemon: sending command: " + DAEMON_COMMAND);
    mCmdWriter.println(DAEMON_COMMAND);

    Utils.runInBg(() -> readMessages(suProcess, null));
    waitForHello();
  }

  private ServerSocket mServer;
  private Socket mClient;

  private void startAdbDaemon(boolean showAdbFailedToast) {
    try {
      mServer = new ServerSocket(0, 0, Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}));
    } catch (IOException e) {
      e.printStackTrace();
      return;
    }

    Future<?> future = Utils.runInBg(this::startServer);

    Adb adb;
    try {
      adb = new Adb(DAEMON_COMMAND + mServer.getLocalPort(), showAdbFailedToast);
    } catch (AdbException e) {
      Log.e(TAG, "startAdbDaemon: " + e.toString());
      return;
    }

    Utils.runInBg(() -> readMessages(null, adb));

    try {
      future.get();
    } catch (ExecutionException | InterruptedException e) {
      e.printStackTrace();
    }
  }

  private void startServer() {
    try {
      mServer.setSoTimeout(5000);
      mClient = mServer.accept();
      mClient.setTcpNoDelay(true);
      mOutStream = mClient.getOutputStream();
      mResponseReader = new BufferedReader(new InputStreamReader(mClient.getInputStream()));
      mCmdWriter = new PrintWriter(mOutStream, true);
    } catch (IOException e) {
      if (e instanceof SocketTimeoutException) {
        Log.e(TAG, "startServer: " + e.toString());
      } else {
        e.printStackTrace();
      }
      return;
    }

    waitForHello();
  }

  private void waitForHello() {
    try {
      String line = mResponseReader.readLine();
      if (line != null && line.startsWith("HELLO ")) {
        mIsRunning = true;
        mIsRoot = Integer.parseInt(line.split(" ")[1]) == 0;
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  public void stopDaemon() {
    if (mIsRunning) {
      mIsRunning = false;
      Utils.runInBg(() -> sendCmdLocked("exit", null));
    }
  }

  private boolean mIsRoot;

  public boolean isRoot() {
    isRunning();
    return mIsRoot;
  }

  public boolean copyStream(int size, String path, InputStream input) {
    if (!isRunning()) {
      return false;
    }
    synchronized (READ_WRITE_LOCK) {
      sendCmd("save " + size + " " + path, null);
      if (Utils.copyStream(input, mOutStream)) {
        try {
          mOutStream.flush();
          return matchResponse("SAVED");
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
      return false;
    }
  }

  public void sendCommand(String cmd) {
    sendCommand(cmd, null);
  }

  public boolean sendCommand(String cmd, String match) {
    if (!isRunning()) {
      return false;
    }
    return sendCmdLocked(cmd, match);
  }

  private boolean sendCmdLocked(String cmd, String match) {
    synchronized (READ_WRITE_LOCK) {
      return sendCmd(cmd, match);
    }
  }

  private boolean sendCmd(String cmd, String match) {
    Log.i(TAG, "Sending command: " + cmd);
    mCmdWriter.println(cmd);

    if (match == null) {
      return true;
    }
    return matchResponse(match);
  }

  private boolean matchResponse(String match) {
    try {
      String line = mResponseReader.readLine();
      Log.i(TAG, "Response: " + line);
      return match.equals(line);
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    }
  }

  private void readMessages(Process process, Adb adb) {
    Reader reader;
    if (process != null) {
      reader = new InputStreamReader(process.getErrorStream());
    } else if (adb != null) {
      reader = adb.getReader();
    } else {
      return;
    }

    try {
      Utils.readProcessLog(new BufferedReader(reader), TAG);
    } catch (IOException e) {
      e.printStackTrace();
    }
    daemonStopped(process, adb);
  }

  private void daemonStopped(Process process, Adb adb) {
    boolean restart = mIsRunning;
    mIsRunning = false;
    Utils.cleanStreams(process, adb, TAG);
    try {
      if (mServer != null) {
        mServer.close();
      }
      if (mClient != null) {
        mClient.close();
      }
    } catch (IOException ignored) {
    }
    if (restart) {
      Log.e(TAG, "readMessages: native daemon died");
      Utils.showToast(R.string.native_daemon_died);
      SystemClock.sleep(5000); // Wait must be smaller than PrivDaemon
      Log.i(TAG, "readMessages: restarting native daemon");
      startDaemon(false);
    }
  }

  private final MutableLiveData<Void> sDrawerChanged = new MutableLiveData<>();

  private void updateDrawer() {
    sDrawerChanged.postValue(null);
  }

  public LiveData<Void> getDrawerChanged() {
    return sDrawerChanged;
  }
}
