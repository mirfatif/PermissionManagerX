package com.mirfatif.permissionmanagerx.privs;

import android.os.Build;
import android.os.SystemClock;
import android.util.Log;
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
import java.io.Reader;
import java.net.Inet4Address;
import java.net.Socket;
import java.util.Arrays;

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

    boolean extractFiles = mMySettings.shouldExtractFiles() || mForceFilesExtraction;
    if (extractFiles) {
      extractFiles();
    }

    String dexFilePath = dexInTmpDir ? TMP_DIR_DEX_PATH : SHARED_DIR_DEX_PATH;
    String daemonScriptPath = dexInTmpDir ? TMP_DIR_SCRIPT_PATH : SHARED_DIR_SCRIPT_PATH;

    if (!SET_PRIV_FILE.exists() || !daemonFilesExist(dexFilePath, daemonScriptPath)) {
      if (!extractFiles) {
        mForceFilesExtraction = true;
        return startDaemon(preferRoot, dexInTmpDir);
      } else {
        Utils.showToast(R.string.files_not_extracted_accessible);
        return false;
      }
    } else if (extractFiles) {
      mMySettings.setFileExtractionTs();
    }

    int daemonUid = mMySettings.getDaemonUid();
    String daemonContext = mMySettings.getDaemonContext();
    boolean useSocket = mMySettings.useSocket();

    String params =
        mMySettings.isDebug()
            + " "
            + daemonUid
            + " "
            + daemonContext
            + " "
            + Utils.getUserId()
            + " "
            + DaemonCmdRcvSvc.CODE_WORD
            + " "
            + dexFilePath
            + " "
            + DAEMON_PACKAGE_NAME
            + " "
            + DAEMON_CLASS_NAME
            + " "
            + BIN_DIR
            + ":"
            + System.getenv("PATH");

    Adb adb = null;
    InputStream inStream = null;
    OutputStream outStream = null;
    BufferedReader inReader;

    if (mPreferRoot) {
      if (useSocket) {
        params += " " + Commands.CREATE_SOCKET;
      }

      Process suProcess = Utils.runCommand(TAG + ": startDaemon", false, Utils.getSu());
      if (suProcess == null) {
        return false;
      }

      inStream = suProcess.getInputStream();
      outStream = suProcess.getOutputStream();
      inReader = new BufferedReader(new InputStreamReader(inStream));
      mCmdWriter = new PrintWriter(outStream, true);

      Log.i(TAG, "startDaemon: sending command: exec sh " + daemonScriptPath);
      mCmdWriter.println("exec sh " + daemonScriptPath);

      Utils.runInBg(() -> readDaemonMessages(suProcess, null));

    } else {
      params += " " + Commands.CREATE_SOCKET;
      useSocket = true;
      try {
        adb = new Adb("exec sh " + daemonScriptPath, true);
      } catch (AdbException e) {
        Log.e(TAG, "startDaemon: " + e.toString());
        return false;
      }
      inReader = new BufferedReader(adb.getReader());
      mCmdWriter = new PrintWriter(adb.getWriter(), true);
    }

    // Daemon script waits and reads parameters from STDIN
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
    mIsSystemUid = obj instanceof Integer && (Integer) obj == 1000;

    mMySettings.setPrivDaemonAlive(true);

    if (mMySettings.shouldStartDaemonLog()) {
      String logCommand = "logcat --pid " + pid;

      if (mPreferRoot) {
        logCommand =
            SET_PRIV_PATH
                + " -u "
                + daemonUid
                + " -g "
                + daemonUid
                + " --context "
                + daemonContext
                + " -- "
                + logCommand;
        if (LogcatService.doLoggingFails(Utils.getSu(), "exec " + logCommand)) {
          return null;
        }
      } else {
        Adb adbLogger;
        try {
          adbLogger = new Adb("exec " + logCommand, true);
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
      Utils.cleanStreams(process, adb, TAG + ": readDaemonMessages");

      if (mMySettings.isPrivDaemonAlive()) {
        mMySettings.setPrivDaemonAlive(false);
        Log.e(TAG, "readDaemonMessages: privileged daemon died");
        Utils.showToast(R.string.priv_daemon_died);
        SystemClock.sleep(5000);
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

  private static final String DAEMON_DEX = DAEMON_PACKAGE_NAME + ".dex";
  private static final String SHARED_DIR_DEX_PATH =
      new File(SHARED_DIR, DAEMON_DEX).getAbsolutePath();
  private static final String TMP_DIR_DEX_PATH = new File(TMP_DIR, DAEMON_DEX).getAbsolutePath();

  private static final String DAEMON_SCRIPT = DAEMON_PACKAGE_NAME + ".sh";
  private static final String SHARED_DIR_SCRIPT_PATH =
      new File(SHARED_DIR, DAEMON_SCRIPT).getAbsolutePath();
  private static final String TMP_DIR_SCRIPT_PATH =
      new File(TMP_DIR, DAEMON_SCRIPT).getAbsolutePath();

  private static final File BIN_DIR =
      new File(App.getContext().getFilesDir(), "bin").getAbsoluteFile();
  private static final String SET_PRIV = "set_priv";
  private static final File SET_PRIV_FILE = new File(BIN_DIR, "set_priv");
  private static final String SET_PRIV_PATH = SET_PRIV_FILE.getAbsolutePath();

  private void extractFiles() {
    extractSetPrivBin(); // Required for extraction to TmpDir.
    extractToTmpDir();
    extractToSharedDir();
    SystemClock.sleep(1000); // Let the files settle
  }

  private final Object SHARED_DIR_LOCK = new Object();

  private void extractToSharedDir() {
    synchronized (SHARED_DIR_LOCK) {
      boolean res = true;
      for (String file : new String[] {DAEMON_DEX, DAEMON_SCRIPT}) {
        String targetFile = file.equals(DAEMON_DEX) ? SHARED_DIR_DEX_PATH : SHARED_DIR_SCRIPT_PATH;
        try (InputStream inStream = App.getContext().getAssets().open(file);
            OutputStream outStream = new FileOutputStream(targetFile)) {
          if (Utils.copyStreamFails(inStream, outStream)) {
            Log.e(TAG, "extractToSharedDir: extracting " + file + " failed");
            res = false;
            continue;
          }
          outStream.flush();
        } catch (IOException e) {
          e.printStackTrace();
          return;
        }
      }
      if (res) {
        Log.i(TAG, "extractToSharedDir: extracted files successfully");
      }
    }
  }

  private final Object TMP_DIR_LOCK = new Object();

  private void extractToTmpDir() {
    synchronized (TMP_DIR_LOCK) {
      Process suProcess = null;
      Adb adb = null;
      OutputStream outStream;
      InputStream inStream = null;
      PrintWriter cmdWriter;
      BufferedReader inReader;

      boolean res = true;

      String set_priv = SET_PRIV_PATH + " -u " + 2000 + " -g " + 2000;
      if (new File("/proc/self/attr/current").exists()) {
        set_priv += " --context u:r:shell:s0";
      }
      set_priv = "exec " + set_priv + " -- sh";

      try {
        for (String file : new String[] {DAEMON_DEX, DAEMON_SCRIPT}) {

          String cmd =
              "exec cat - >" + (file.equals(DAEMON_DEX) ? TMP_DIR_DEX_PATH : TMP_DIR_SCRIPT_PATH);

          if (mMySettings.isRootGranted()) {
            suProcess = Utils.runCommand(TAG + ": extractToTmpDir", true, Utils.getSu());
            if (suProcess == null) {
              Log.e(TAG, "extractToTmpDir: extraction failed");
              res = false;
              continue;
            }
            outStream = suProcess.getOutputStream();
            cmdWriter = new PrintWriter(outStream, true);
            Log.i(TAG, "extractToTmpDir: sending command: " + set_priv);
            cmdWriter.println(set_priv);
            Log.i(TAG, "extractToTmpDir: sending command: " + cmd);
            cmdWriter.println(cmd);
            inReader = new BufferedReader(new InputStreamReader(suProcess.getInputStream()));

          } else if (mMySettings.isAdbConnected()) {
            adb = new Adb("exec sh -c '" + cmd + "'", true);
            outStream = adb.getOutputStream();
            inReader = new BufferedReader(adb.getReader());
          } else {
            return;
          }

          Reader finalReader = inReader;
          Utils.runInBg(
              () -> {
                try {
                  Utils.readProcessLog(new BufferedReader(finalReader), TAG + ": extractToTmpDir");
                } catch (IOException ignored) {
                }
              });

          inStream = App.getContext().getAssets().open(file);
          if (Utils.copyStreamFails(inStream, outStream)) {
            Log.e(TAG, "extractToTmpDir: extracting " + file + " failed");
            res = false;
            continue;
          }
          outStream.flush();
          outStream.close();
          try {
            if (suProcess != null) {
              // Let the cat process terminate itself.
              suProcess.waitFor();
            }
          } catch (InterruptedException ignored) {
          }
        }
      } catch (IOException e) {
        e.printStackTrace();
        return;
      } catch (AdbException e) {
        Log.e(TAG, "extractToTmpDir: " + e.toString());
        return;
      } finally {
        try {
          if (inStream != null) {
            inStream.close();
          }
        } catch (IOException ignored) {
        }
        Utils.cleanStreams(suProcess, adb, TAG + ": extractToTmpDir");
      }
      if (res) {
        Log.i(TAG, "extractToTmpDir: extracted files successfully");
      }
    }
  }

  private final Object SET_PRIV_LOCK = new Object();

  // Required to start daemon if running as root (ADBD or su)
  private void extractSetPrivBin() {
    synchronized (SET_PRIV_LOCK) {
      if (!BIN_DIR.exists() && !BIN_DIR.mkdirs()) {
        Log.e(TAG, "extractSetPrivBin: could not create directory " + BIN_DIR);
        return;
      }

      String arch = "_arm";
      String supportedABIs = Arrays.toString(Build.SUPPORTED_ABIS).toLowerCase();
      if (supportedABIs.contains("x86")) {
        arch = "_x86";
      } else if (!supportedABIs.contains("arm")) {
        Log.e(TAG, "extractSetPrivBin: arch not supported " + supportedABIs);
        return;
      }

      try (InputStream inputStream = App.getContext().getAssets().open(SET_PRIV + arch);
          OutputStream outputStream = new FileOutputStream(SET_PRIV_FILE)) {
        if (Utils.copyStreamFails(inputStream, outputStream)) {
          Log.e(TAG, "extractSetPrivBin: extracting " + SET_PRIV + " failed");
          return;
        }
        outputStream.flush();
      } catch (IOException e) {
        e.printStackTrace();
        return;
      }

      String command = "exec chmod 0755 " + SET_PRIV_FILE;
      Utils.runCommand(TAG + ": extractSetPrivBin", null, command, "sh");

      Log.i(TAG, "extractSetPrivBin: extracted binary successfully");
    }
  }

  private boolean daemonFilesExist(String dex, String script) {
    String EXIST = "EXIST";
    String cmd = "test -f " + dex + " && test -f " + script + " && echo " + EXIST + " ; exit $?";
    if (mPreferRoot) {
      return Utils.runCommand(TAG + ": filesExist", EXIST, cmd, Utils.getSu());
    } else {
      return Adb.runCommand("exec sh -c '" + cmd + "'", true, TAG + ": filesExist", EXIST);
    }
  }
}
