package com.mirfatif.permissionmanagerx.privs;

import android.os.Build;
import android.os.SystemClock;
import android.util.Log;
import android.widget.Toast;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.Adb.AdbException;
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

  private PrivDaemonHandler() {}

  private final MySettings mMySettings = MySettings.getInstance();

  private static final String DAEMON_PACKAGE_NAME = "com.mirfatif.privdaemon";
  private static final String DAEMON_CLASS_NAME = "PrivDaemon";
  private static final String DAEMON_DEX = "com.mirfatif.privdaemon.dex";
  private static final String DAEMON_SCRIPT = "com.mirfatif.privdaemon.sh";

  private Process suProcess;
  private Adb adb;
  private OutputStream outStream;
  private InputStream inStream;
  private BufferedReader inReader;

  private final File binDir = new File(App.getContext().getFilesDir(), "bin");

  private PrintWriter cmdWriter;
  private ObjectInputStream responseInStream;

  public synchronized Boolean startDaemon(boolean preferRoot) {
    boolean dexInTmpDir = mMySettings.dexInTmpDir();
    Boolean res = startDaemon(preferRoot, dexInTmpDir);
    if (res == null || res) {
      return true;
    }
    res = startDaemon(preferRoot, !dexInTmpDir);
    if (res == null || res) {
      Utils.runInFg(
          () ->
              Toast.makeText(App.getContext(), R.string.dex_location_changed, Toast.LENGTH_LONG)
                  .show());
      mMySettings.setDexInTmpDir(!dexInTmpDir);
    }
    return res;
  }

  private Boolean startDaemon(boolean preferRoot, boolean dexInTmpDir) {
    // Required if running as root (ADBD or su)
    if (!extractBin()) {
      return false;
    }

    boolean isAdbConnected = mMySettings.isAdbConnected();
    mPreferRoot = mMySettings.isRootGranted() && (preferRoot || !isAdbConnected);

    String dexFilePath;
    String daemonScriptPath;

    if (dexInTmpDir) {
      String prefix = "/data/local/tmp/";
      dexFilePath = prefix + DAEMON_DEX;
      daemonScriptPath = prefix + DAEMON_SCRIPT;
      if (!extractToTmpDir(dexFilePath, daemonScriptPath, isAdbConnected)) {
        return false;
      }

    } else {
      File prefix = App.getContext().getExternalFilesDir(null);
      dexFilePath = new File(prefix, DAEMON_DEX).getAbsolutePath();
      daemonScriptPath = new File(prefix, DAEMON_SCRIPT).getAbsolutePath();
      if (!extractToSharedStorage(dexFilePath, daemonScriptPath)) {
        return false;
      }
    }

    // Let the files be saved
    SystemClock.sleep(500);

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
            + dexFilePath
            + " "
            + DAEMON_PACKAGE_NAME
            + " "
            + DAEMON_CLASS_NAME
            + " "
            + binDir
            + ":"
            + System.getenv("PATH");

    if (mPreferRoot) {
      if (useSocket) {
        params += " " + Commands.CREATE_SOCKET;
      }

      suProcess = Utils.runCommand(TAG + ": startDaemon()", false, "su");
      if (suProcess == null) {
        return false;
      }

      inStream = suProcess.getInputStream();
      outStream = suProcess.getOutputStream();
      inReader = new BufferedReader(new InputStreamReader(inStream));
      cmdWriter = new PrintWriter(outStream, true);

      Log.i(TAG, "startDaemon(): sending command: exec sh " + daemonScriptPath);
      cmdWriter.println("exec sh " + daemonScriptPath);

      Process finalSuProcess = suProcess;
      Utils.runInBg(() -> readDaemonMessages(finalSuProcess, null));

    } else if (isAdbConnected) {
      params += " " + Commands.CREATE_SOCKET;
      useSocket = true;
      try {
        adb = new Adb("exec sh " + daemonScriptPath, true);
      } catch (AdbException e) {
        Log.e(TAG, "startDaemon(): " + e.toString());
        return false;
      }
      inReader = new BufferedReader(adb.getReader());
      cmdWriter = new PrintWriter(adb.getWriter(), true);
    } else {
      return false;
    }

    // Daemon script waits and reads parameters from STDIN
    Log.i(TAG, "startDaemon(): sending params");
    cmdWriter.println(params);

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
        Log.i(TAG, "startDaemon(): " + DAEMON_CLASS_NAME + ": " + line);
      }

      if (pid <= 0 || (useSocket && port <= 0)) {
        Log.e(TAG, "startDaemon(): bad or no response from privileged daemon");
        return false;
      }

      Log.i(TAG, "startDaemon(): sending command: " + Commands.GET_READY);
      cmdWriter.println(Commands.GET_READY);

      // We have single input stream to read in case of ADB, so
      // we couldn't read log messages before receiving PID and port number.
      if (!mPreferRoot) {
        Adb finalAdb = adb;
        Utils.runInBg(() -> readDaemonMessages(null, finalAdb));
      }

      if (!useSocket) {
        responseInStream = new ObjectInputStream(inStream);
      } else {
        // AdbLib redirects stdErr to stdIn. So create direct Socket.
        // Also in case of ADB binary, ADB over Network speed sucks
        Socket socket = new Socket(Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}), port);
        socket.setTcpNoDelay(true);

        cmdWriter = new PrintWriter(socket.getOutputStream(), true);
        responseInStream = new ObjectInputStream(socket.getInputStream());

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
      Object obj = responseInStream.readObject();
      if (!(obj instanceof String) || !obj.equals(Commands.GET_READY)) {
        Log.e(TAG, "startDaemon(): bad response from privileged daemon");
        return false;
      }
    } catch (IOException | ClassNotFoundException e) {
      e.printStackTrace();
      Log.e(TAG, "startDaemon(): error starting privileged daemon");
      return false;
    }

    mMySettings.setPrivDaemonAlive(true);

    if (mMySettings.shouldStartDaemonLog()) {
      String logCommand = "logcat --pid " + pid;

      if (mPreferRoot) {
        logCommand =
            binDir
                + "/set_priv -u "
                + daemonUid
                + " -g "
                + daemonUid
                + " --context "
                + daemonContext
                + " -- "
                + logCommand;
        if (LogcatService.doLoggingFails("su", "exec " + logCommand)) {
          return null;
        }
      } else {
        Adb adbLogger;
        try {
          adbLogger = new Adb("exec " + logCommand, true);
        } catch (AdbException e) {
          Log.e(TAG, "startDaemon(): " + e.toString());
          return null;
        }
        Utils.runInBg(() -> LogcatService.readLogcatStream(null, adbLogger));
      }
    }

    return true;
  }

  private boolean extractBin() {
    String binary = "set_priv";
    File binaryPath = new File(binDir, binary);
    if (!binaryPath.exists()) {
      if (!binDir.exists() && !binDir.mkdirs()) {
        Log.e(TAG, "extractBin(): could not create directory " + binDir);
        return false;
      }

      long lastUpdated = new File(App.getContext().getApplicationInfo().sourceDir).lastModified();
      if (lastUpdated > binaryPath.lastModified()) {
        String arch = "_arm";
        String supportedABIs = Arrays.toString(Build.SUPPORTED_ABIS).toLowerCase();
        if (supportedABIs.contains("x86")) {
          arch = "_x86";
        } else if (!supportedABIs.contains("arm")) {
          Log.e(TAG, "extractBin(): arch not supported " + supportedABIs);
          return false;
        }

        try (InputStream inputStream = App.getContext().getAssets().open(binary + arch);
            OutputStream outputStream = new FileOutputStream(binaryPath)) {
          if (Utils.copyStreamFails(inputStream, outputStream)) {
            Log.e(TAG, "extractBin(): extracting " + binary + " failed");
            return false;
          }
          String command = "chmod 0755 " + binaryPath;
          Process p = Runtime.getRuntime().exec(command);
          if (p.waitFor() != 0) {
            Log.e(TAG, "extractBin(): " + command + " failed");
            return false;
          }
        } catch (IOException | InterruptedException e) {
          e.printStackTrace();
          return false;
        }
      }
    }
    return true;
  }

  private boolean extractToTmpDir(String dex, String script, boolean isAdb) {
    try {
      for (String file : new String[] {DAEMON_DEX, DAEMON_SCRIPT}) {

        String cmd = "exec cat - >" + (file.equals(DAEMON_DEX) ? dex : script);

        if (mPreferRoot) {
          String set_priv = binDir + "/set_priv -u " + 2000 + " -g " + 2000;
          if (new File("/proc/self/ns/mnt").exists()) {
            set_priv += " --context u:r:shell:s0";
          }
          set_priv = "exec " + set_priv + " -- sh";

          suProcess = Utils.runCommand(TAG + ": extractToTmpDir()", true, "su");
          if (suProcess == null) {
            return false;
          }
          outStream = suProcess.getOutputStream();
          cmdWriter = new PrintWriter(outStream, true);
          Log.i(TAG, "extractToTmpDir(): sending command: " + set_priv);
          cmdWriter.println(set_priv);
          Log.i(TAG, "extractToTmpDir(): sending command: " + cmd);
          cmdWriter.println(cmd);
          inReader = new BufferedReader(new InputStreamReader(suProcess.getInputStream()));

        } else if (isAdb) {
          adb = new Adb("exec sh -c '" + cmd + "'", true);
          outStream = adb.getOutputStream();
          cmdWriter = new PrintWriter(outStream, true);
          inReader = new BufferedReader(adb.getReader());

        } else {
          Log.e(TAG, "extractToTmpDir(): cannot start privileged daemon without root or ADB shell");
          return false;
        }

        Reader finalReader = inReader;
        Utils.runInBg(
            () -> {
              try {
                Utils.readProcessLog(new BufferedReader(finalReader), TAG + ": extractToTmpDir()");
              } catch (IOException ignored) {
              }
            });

        inStream = App.getContext().getAssets().open(file);
        if (Utils.copyStreamFails(inStream, outStream)) {
          Log.e(TAG, "extractToTmpDir(): extracting " + file + " failed");
          return false;
        }
        outStream.flush();
        outStream.close();
      }
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    } catch (AdbException e) {
      Log.e(TAG, "extractToTmpDir(): " + e.toString());
      return false;
    } finally {
      try {
        if (inStream != null) {
          inStream.close();
        }
        if (outStream != null) {
          outStream.close();
        }
      } catch (IOException ignored) {
      }
      Utils.cleanProcess(inReader, suProcess, adb, TAG + ": extractToTmpDir()");
      suProcess = null;
      adb = null;
      inStream = null;
      outStream = null;
      inReader = null;
      cmdWriter = null;
    }
    return true;
  }

  private boolean extractToSharedStorage(String dex, String script) {
    for (String file : new String[] {DAEMON_DEX, DAEMON_SCRIPT}) {
      try {
        inStream = App.getContext().getAssets().open(file);
        outStream = new FileOutputStream(file.equals(DAEMON_DEX) ? dex : script);
        if (Utils.copyStreamFails(inStream, outStream)) {
          Log.e(TAG, "extractToSharedStorage(): extracting " + file + " failed");
          return false;
        }
        outStream.flush();
        outStream.close();
      } catch (IOException e) {
        e.printStackTrace();
        return false;
      } finally {
        try {
          if (inStream != null) {
            inStream.close();
          }
          if (outStream != null) {
            outStream.close();
          }
        } catch (IOException ignored) {
        }
        inStream = null;
        outStream = null;
      }
    }
    return true;
  }

  private boolean mPreferRoot;

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
      Utils.cleanProcess(reader, process, adb, TAG + ": readDaemonMessages()");

      if (MySettings.getInstance().isPrivDaemonAlive()) {
        MySettings.getInstance().setPrivDaemonAlive(false);
        Log.e(TAG, "readDaemonMessages(): privileged daemon died");
        SystemClock.sleep(5000);
        Log.i(TAG, "readDaemonMessages(): restarting privileged daemon");
        startDaemon(mPreferRoot);
      }
    }
  }

  private final Object SEND_REQ_LOCK = new Object();

  public Object sendRequest(String request) {
    synchronized (SEND_REQ_LOCK) {
      MySettings mySettings = MySettings.getInstance();
      if (!mySettings.isPrivDaemonAlive()) {
        Log.e(TAG, "sendRequest(): " + request + ": Privileged daemon is dead");
        return null;
      }

      if (cmdWriter == null || responseInStream == null) {
        Log.e(TAG, "sendRequest(): cmdWriter or ResponseReader is null");
        return null;
      }

      // To avoid getting restarted
      if (request.equals(Commands.SHUTDOWN)) {
        mySettings.setPrivDaemonAlive(false);
      }

      cmdWriter.println(request);

      if (request.equals(Commands.SHUTDOWN)) {
        return null;
      }

      try {
        return responseInStream.readObject();
      } catch (IOException | ClassNotFoundException e) {
        e.printStackTrace();

        Log.e(TAG, "sendRequest(): restarting privileged daemon");
        cmdWriter.println(Commands.SHUTDOWN);

        return null;
      }
    }
  }
}
