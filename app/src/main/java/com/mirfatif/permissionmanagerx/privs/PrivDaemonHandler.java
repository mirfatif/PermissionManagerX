package com.mirfatif.permissionmanagerx.privs;

import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.PMX_BIN_PATH;
import static com.mirfatif.permissionmanagerx.util.Utils.UID_SHELL;
import static com.mirfatif.permissionmanagerx.util.Utils.UID_SYSTEM;

import android.os.SystemClock;
import android.util.Log;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.svc.DaemonCmdRcvSvc;
import com.mirfatif.permissionmanagerx.svc.LogcatService;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.Inet4Address;
import java.net.Socket;

public class PrivDaemonHandler {

  private static final String TAG = "PrivDaemonHandler";

  private static PrivDaemonHandler mPrivDaemonHandler;

  public static synchronized PrivDaemonHandler getInstance() {
    if (mPrivDaemonHandler == null) {
      mPrivDaemonHandler = new PrivDaemonHandler();
    }
    return mPrivDaemonHandler;
  }

  private final MySettings mMySettings = MySettings.getInstance();
  private final NativeDaemon mRootDaemon = NativeDaemon.rootInstance();
  private final NativeDaemon mAdbDaemon = NativeDaemon.adbInstance();
  private static final String DAEMON_GROUPS = "2000,3003,3009,1015,1023,1078,9997";

  private PrivDaemonHandler() {}

  private boolean mForceFilesExtraction = false;
  private boolean mPreferRoot;
  private PrintWriter mCmdWriter;
  private ObjectInputStream mResponseInStream;

  public synchronized Boolean startDaemon(boolean preferRoot) {
    if (mMySettings.isPrivDaemonAlive()) {
      Log.e(TAG, "startDaemon: daemon already running");
      return false;
    }
    boolean dexInTmpDir = mMySettings.dexInTmpDir();
    Boolean res = startDaemon(preferRoot, dexInTmpDir);
    if (res == null || res) {
      return true;
    }
    res = startDaemon(preferRoot, !dexInTmpDir);
    if (res == null || res) {
      Utils.showToast(R.string.dex_location_changed);
      mMySettings.setDexInTmpDir(!dexInTmpDir);
    }
    return res;
  }

  private Boolean startDaemon(boolean preferRoot, boolean dexInTmpDir) {
    boolean isAdbConnected = mMySettings.isAdbConnected();
    mPreferRoot = mMySettings.isRootGranted() && (preferRoot || !isAdbConnected);

    if (!mPreferRoot && !isAdbConnected) {
      Log.e(TAG, "startDaemon: cannot start privileged daemon without root or ADB shell");
      return false;
    }

    boolean extractDex = mMySettings.shouldExtractFiles() || mForceFilesExtraction;
    if (extractDex) {
      extractToTmpDir();
      extractToSharedDir();
      mForceFilesExtraction = false;
    }

    String dexFilePath = dexInTmpDir ? TMP_DIR_DEX_PATH : SHARED_DIR_DEX_PATH;

    if (!dexExists(dexFilePath)) {
      if (!extractDex) {
        mForceFilesExtraction = true;
        return startDaemon(preferRoot, dexInTmpDir);
      } else {
        Utils.showToast(R.string.files_not_extracted_accessible);
        return false;
      }
    } else if (extractDex) {
      mMySettings.setFileExtractionTs();
    }

    String daemonContext = mMySettings.getDaemonContext();

    String vmName = DAEMON_PACKAGE_NAME + ".pmx" + (BuildConfig.DEBUG ? ".debug" : "");
    String vmClass = DAEMON_PACKAGE_NAME + "." + DAEMON_CLASS_NAME;
    String vmCmd = "app_process / --nice-name=" + vmName + " " + vmClass;
    if (mPreferRoot || mAdbDaemon.isRoot()) {
      vmCmd =
          PMX_BIN_PATH
              + " --ns 1 --set-cg --rcaps "
              + (daemonContext.equals(MySettings.CONTEXT_DEFAULT) ? "" : "--cxt " + daemonContext)
              + " -u "
              + mMySettings.getDaemonUid()
              + " -g "
              + mMySettings.getDaemonUid()
              + " --groups "
              + DAEMON_GROUPS
              + " -- "
              + vmCmd;
    }
    vmCmd = "export CLASSPATH=" + dexFilePath + "; exec " + vmCmd;

    Adb adb = null;
    InputStream inStream = null;
    OutputStream outStream = null;
    BufferedReader inReader;

    if (mPreferRoot) {
      Process suProcess = Utils.runCommand(TAG + ": startDaemon", false, Utils.getSu());
      if (suProcess == null) {
        return false;
      }

      inStream = suProcess.getInputStream();
      outStream = suProcess.getOutputStream();
      inReader = new BufferedReader(new InputStreamReader(inStream));
      mCmdWriter = new PrintWriter(outStream, true);

      Log.i(TAG, "startDaemon: sending command: " + vmCmd);
      mCmdWriter.println(vmCmd);

      Utils.runInBg(() -> readDaemonMessages(suProcess, null));

    } else {
      try {
        adb = new Adb(vmCmd, true);
      } catch (AdbException e) {
        Log.e(TAG, "startDaemon: " + e.toString());
        return false;
      }
      inReader = new BufferedReader(adb.getReader());
      mCmdWriter = new PrintWriter(adb.getWriter(), true);
    }

    boolean useSocket = !mPreferRoot || mMySettings.useSocket();

    String params =
        mMySettings.isDebug()
            + " "
            + useSocket
            + " "
            + Utils.getUserId()
            + " "
            + BuildConfig.APPLICATION_ID
            + " "
            + DaemonCmdRcvSvc.CODE_WORD;

    // Daemon waits and reads parameters from STDIN
    Log.i(TAG, "startDaemon: sending params");
    mCmdWriter.println(params);

    int pid = 0;
    int port = 0;
    try {
      String line;
      while ((line = inReader.readLine()) != null) {
        if (line.startsWith(Commands.HELLO)) {
          pid = Integer.parseInt(line.split(":")[1]);
          port = Integer.parseInt(line.split(":")[2]);
          break;
        }
        Log.i(TAG, "startDaemon: " + DAEMON_CLASS_NAME + ": " + line);
      }

      if (pid <= 0 || (useSocket && port <= 0)) {
        Log.e(TAG, "startDaemon: bad or no response from privileged daemon");
        return false;
      }

      Log.i(TAG, "startDaemon: sending command: " + Commands.GET_READY);
      mCmdWriter.println(Commands.GET_READY);

      // We have single input stream to read in case of ADB, so
      // we couldn't read log messages before receiving PID and port number.
      if (!mPreferRoot) {
        Adb finalAdb = adb;
        Utils.runInBg(() -> readDaemonMessages(null, finalAdb));
      }

      if (!useSocket) {
        mResponseInStream = new ObjectInputStream(inStream);
      } else {
        // AdbLib redirects stdErr to stdIn. So create direct Socket.
        // Also in case of ADB binary, ADB over Network speed sucks
        Socket socket = new Socket(Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}), port);
        socket.setTcpNoDelay(true);

        mCmdWriter = new PrintWriter(socket.getOutputStream(), true);
        mResponseInStream = new ObjectInputStream(socket.getInputStream());

        // cmdWriter and responseInStream both are using socket, so close su process streams.
        if (mPreferRoot) {
          if (outStream != null) {
            outStream.close();
          }
          if (inStream != null) {
            inStream.close();
          }
        }
      }

      // Get response to GET_READY command
      Object obj = mResponseInStream.readObject();
      if (!(obj instanceof String) || !obj.equals(Commands.GET_READY)) {
        Log.e(TAG, "startDaemon: bad response from privileged daemon");
        return false;
      }
    } catch (IOException | ClassNotFoundException e) {
      e.printStackTrace();
      Log.e(TAG, "startDaemon: error starting privileged daemon");
      return false;
    }

    // Even with ADB we may get System UID if ADBD is running as root.
    Object obj = sendRequest(Commands.GET_UID, true);
    mIsSystemUid = obj instanceof Integer && (Integer) obj == UID_SYSTEM;

    mMySettings.setPrivDaemonAlive(true);

    if (mMySettings.shouldStartDaemonLog()) {
      String logCommand = "exec logcat --pid " + pid;

      if (mPreferRoot) {
        if (!LogcatService.doLogging(Utils.getSu(), logCommand)) {
          return null;
        }
      } else {
        Adb adbLogger;
        try {
          adbLogger = new Adb(logCommand, true);
        } catch (AdbException e) {
          Log.e(TAG, "startDaemon: " + e.toString());
          return null;
        }
        Utils.runInBg(() -> LogcatService.readLogcatStream(null, adbLogger));
      }
    }

    return true;
  }

  private void readDaemonMessages(Process process, Adb adb) {
    BufferedReader reader;
    if (process != null) {
      reader = new BufferedReader(new InputStreamReader(process.getErrorStream()));
    } else if (adb != null) {
      reader = new BufferedReader(adb.getReader());
    } else {
      return;
    }

    try {
      Utils.readProcessLog(reader, DAEMON_CLASS_NAME);
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      boolean restart = mMySettings.isPrivDaemonAlive();
      mMySettings.setPrivDaemonAlive(false);
      Utils.cleanStreams(process, adb, TAG + ": readDaemonMessages");

      if (restart) {
        Log.e(TAG, "readDaemonMessages: privileged daemon died");
        Utils.showToast(R.string.priv_daemon_died);
        SystemClock.sleep(10000); // Wait must be greater than NativeDaemon
        Log.i(TAG, "readDaemonMessages: restarting privileged daemon");
        startDaemon(mPreferRoot);
      }
    }
  }

  private final Object SEND_REQ_LOCK = new Object();

  public Object sendRequest(String request) {
    return sendRequest(request, mMySettings.isPrivDaemonAlive());
  }

  private Object sendRequest(String request, boolean isPrivDaemonAlive) {
    synchronized (SEND_REQ_LOCK) {
      if (!isPrivDaemonAlive) {
        Log.e(TAG, "sendRequest: " + request + ": Privileged daemon is dead");
        return null;
      }

      if (mCmdWriter == null || mResponseInStream == null) {
        Log.e(TAG, "sendRequest: cmdWriter or ResponseReader is null");
        return null;
      }

      // To avoid getting restarted
      if (request.equals(Commands.SHUTDOWN)) {
        mMySettings.setPrivDaemonAlive(false);
      }

      mCmdWriter.println(request);

      if (request.equals(Commands.SHUTDOWN)) {
        return null;
      }

      try {
        return mResponseInStream.readObject();
      } catch (IOException | ClassNotFoundException e) {
        e.printStackTrace();

        Log.e(TAG, "sendRequest: restarting privileged daemon");
        mCmdWriter.println(Commands.SHUTDOWN);

        return null;
      }
    }
  }

  private boolean mIsSystemUid = false;

  public boolean isSystemUid() {
    return mIsSystemUid;
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////// FILE EXTRACTION ////////////////////////
  //////////////////////////////////////////////////////////////////

  private static final String DAEMON_PACKAGE_NAME = "com.mirfatif.privdaemon";
  private static final String DAEMON_CLASS_NAME = "PrivDaemon";

  private static final String SHARED_DIR =
      App.getContext().getExternalFilesDir(null).getAbsolutePath();
  private static final String TMP_DIR = "/data/local/tmp/";

  private static final String DAEMON_DEX = DAEMON_PACKAGE_NAME + ".pmx.dex";
  private static final String SHARED_DIR_DEX_PATH =
      new File(SHARED_DIR, DAEMON_DEX).getAbsolutePath();
  private static final String TMP_DIR_DEX_PATH = new File(TMP_DIR, DAEMON_DEX).getAbsolutePath();

  private final Object SHARED_DIR_LOCK = new Object();

  private void extractToSharedDir() {
    synchronized (SHARED_DIR_LOCK) {
      try (InputStream inStream = App.getContext().getAssets().open(DAEMON_DEX);
          OutputStream outStream = new FileOutputStream(SHARED_DIR_DEX_PATH)) {
        if (!Utils.copyStream(inStream, outStream)) {
          Log.e(TAG, "extractToSharedDir: extracting " + DAEMON_DEX + " failed");
        } else {
          outStream.flush();
          Log.i(TAG, "extractToSharedDir: extracted file successfully");
        }
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  private final Object TMP_DIR_LOCK = new Object();

  private void extractToTmpDir() {
    synchronized (TMP_DIR_LOCK) {
      NativeDaemon daemon;
      if (mMySettings.isRootGranted()) {
        daemon = mRootDaemon;
      } else if (mMySettings.isAdbConnected()) {
        daemon = mAdbDaemon;
      } else {
        return;
      }

      try (InputStream inStream1 = App.getContext().getAssets().open(DAEMON_DEX);
          InputStream inStream2 = App.getContext().getAssets().open(DAEMON_DEX)) {
        int size = 0;
        while (inStream1.read() != -1) {
          size++;
        }

        if (!daemon.copyStream(size, TMP_DIR_DEX_PATH, inStream2)) {
          Log.e(TAG, "extractToTmpDir: extracting " + DAEMON_DEX + " failed");
          return;
        }
        if (daemon.isRoot()) {
          daemon.sendCommand(
              "perm " + UID_SHELL + " " + UID_SHELL + " 0644 PARENT " + TMP_DIR_DEX_PATH);
        }
      } catch (IOException e) {
        e.printStackTrace();
        return;
      }

      Log.i(TAG, "extractToTmpDir: extracted file successfully");
    }
  }

  private boolean dexExists(String dexPath) {
    NativeDaemon daemon;
    if (mMySettings.isRootGranted()) {
      daemon = mRootDaemon;
    } else if (mMySettings.isAdbConnected()) {
      daemon = mAdbDaemon;
    } else {
      return false;
    }
    return daemon.sendCommand("exist " + dexPath, "EXIST");
  }
}
