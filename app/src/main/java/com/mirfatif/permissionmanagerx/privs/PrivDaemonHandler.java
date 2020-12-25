package com.mirfatif.permissionmanagerx.privs;

import android.os.Build;
import android.os.SystemClock;
import android.util.Log;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
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
  private static final String DAEMON_PACKAGE_NAME = "com.mirfatif.privdaemon";
  private static final String DAEMON_CLASS_NAME = "PrivDaemon";

  private PrintWriter cmdWriter;
  private ObjectInputStream responseInStream;

  private static PrivDaemonHandler mPrivDaemonHandler;

  public static synchronized PrivDaemonHandler getInstance() {
    if (mPrivDaemonHandler == null) {
      mPrivDaemonHandler = new PrivDaemonHandler();
    }
    return mPrivDaemonHandler;
  }

  private PrivDaemonHandler() {}

  public Boolean startDaemon() {

    // Required if running as root (ADBD or su)
    File binDir = new File(App.getContext().getFilesDir(), "bin");
    String binary = "set_priv";
    File binaryPath = new File(binDir, binary);
    if (!binaryPath.exists()) {
      if (!binDir.exists() && !binDir.mkdirs()) {
        Log.e(TAG, "Could not create directory " + binDir);
        return false;
      }

      long lastUpdated = new File(App.getContext().getApplicationInfo().sourceDir).lastModified();
      if (lastUpdated > binaryPath.lastModified()) {
        String arch = "_arm";
        String supportedABIs = Arrays.toString(Build.SUPPORTED_ABIS).toLowerCase();
        if (supportedABIs.contains("x86")) {
          arch = "_x86";
        } else if (!supportedABIs.contains("arm")) {
          Log.e(TAG, "Arch not supported " + supportedABIs);
          return false;
        }

        try (InputStream inputStream = App.getContext().getAssets().open(binary + arch);
            OutputStream outputStream = new FileOutputStream(binaryPath)) {
          if (Utils.copyStreamFails(inputStream, outputStream)) {
            Log.e(TAG, "Extracting " + binary + " failed");
            return false;
          }
          String command = "chmod 0755 " + binaryPath;
          Process p = Runtime.getRuntime().exec(command);
          if (p.waitFor() != 0) {
            Log.e(TAG, command + " failed");
            return false;
          }
        } catch (IOException | InterruptedException e) {
          e.printStackTrace();
          return false;
        }
      }
    }

    MySettings mySettings = MySettings.getInstance();
    boolean isRootGranted = mySettings.isRootGranted();

    String daemonDex = "daemon.dex";
    String daemonScript = "daemon.sh";
    String prefix = "/data/local/tmp/com.mirfatif.priv";
    String dexFilePath = prefix + daemonDex;
    String daemonScriptPath = prefix + daemonScript;

    Process suProcess = null;
    Adb adb = null;
    OutputStream outStream = null;
    InputStream inStream = null;
    BufferedReader inReader = null;

    try {
      for (String file : new String[] {daemonDex, daemonScript}) {
        String cmd;
        if (file.equals(daemonDex)) {
          cmd = "sh -c 'exec cat - >" + dexFilePath + "'";
        } else {
          cmd = "sh -c 'exec cat - >" + daemonScriptPath + "'";
        }

        if (isRootGranted) {
          String set_priv = binDir + "/set_priv -u " + 2000 + " -g " + 2000;
          if (new File("/proc/self/ns/mnt").exists()) {
            set_priv += " --context u:r:shell:s0";
          }
          cmd = "exec " + set_priv + " -- " + cmd;

          suProcess = Utils.runCommand("su -c " + cmd, TAG, true);
          if (suProcess == null) {
            return false;
          }
          outStream = suProcess.getOutputStream();
          inReader = new BufferedReader(new InputStreamReader(suProcess.getInputStream()));
        } else if (mySettings.isAdbConnected()) {
          adb = new Adb("exec " + cmd);
          outStream = adb.getOutputStream();
          inReader = new BufferedReader(adb.getReader());
        } else {
          Log.e(TAG, "Cannot start privileged daemon without root or ADB shell");
          return false;
        }

        Reader finalReader = inReader;
        Utils.runInBg(
            () -> {
              try {
                readProcessLog(new BufferedReader(finalReader), "DaemonFilesExtraction");
              } catch (IOException ignored) {
              }
            });

        inStream = App.getContext().getAssets().open(file);
        if (Utils.copyStreamFails(inStream, outStream)) {
          Log.e(TAG, "Extracting " + file + " failed");
          return false;
        }
        outStream.flush();
        outStream.close();
      }
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    } finally {
      try {
        if (outStream != null) {
          outStream.close();
        }
        if (inStream != null) {
          inStream.close();
        }
      } catch (IOException ignored) {
      }
      Utils.cleanProcess(inReader, suProcess, adb, "DaemonFilesExtraction");
      adb = null;
      outStream = null;
      inStream = null;
    }

    // Let the files be saved
    SystemClock.sleep(500);

    int daemonUid = mySettings.getDaemonUid();
    String daemonContext = mySettings.getDaemonContext();
    boolean useSocket = mySettings.useSocket();

    String params =
        mySettings.isDebug()
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

    String daemonCommand = "exec sh " + daemonScriptPath;

    if (isRootGranted) {
      if (useSocket) {
        params += " " + Commands.CREATE_SOCKET;
      }

      suProcess = Utils.runCommand("su -c " + daemonCommand, TAG, false);
      if (suProcess == null) {
        return false;
      }

      inStream = suProcess.getInputStream();
      outStream = suProcess.getOutputStream();
      inReader = new BufferedReader(new InputStreamReader(inStream));
      cmdWriter = new PrintWriter(outStream, true);
      Process finalSuProcess = suProcess;
      Utils.runInBg(() -> readDaemonMessages(finalSuProcess, null));

    } else if (mySettings.isAdbConnected()) {
      params += " " + Commands.CREATE_SOCKET;
      useSocket = true;
      try {
        adb = new Adb(daemonCommand);
      } catch (IOException e) {
        e.printStackTrace();
        return false;
      }
      inReader = new BufferedReader(adb.getReader());
      cmdWriter = new PrintWriter(adb.getWriter(), true);
    } else {
      return false;
    }

    // shell script reads from STDIN
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
        Log.i(DAEMON_CLASS_NAME, line);
      }

      if (pid <= 0 || (useSocket && port <= 0)) {
        Log.e(TAG, "Bad or no response from privileged daemon");
        return false;
      }

      cmdWriter.println(Commands.GET_READY);

      // We have single input stream to read in case of ADB, so
      // we couldn't read log messages before receiving PID and port number.
      if (!isRootGranted) {
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
        if (isRootGranted) {
          outStream.close();
          inStream.close();
        }
      }

      // get response to GET_READY command
      Object obj = responseInStream.readObject();
      if (!(obj instanceof String) || !obj.equals(Commands.GET_READY)) {
        Log.e(TAG, "Bad response from privileged daemon");
        return false;
      }
    } catch (IOException | ClassNotFoundException e) {
      e.printStackTrace();
      Log.e(TAG, "Error starting privileged daemon");
      return false;
    }

    mySettings.setPrivDaemonAlive(true);

    if (mySettings.hasLoggingStarted()) {
      mySettings.setLoggingFullyStarted();
      String logCommand = "logcat --pid " + pid;

      if (isRootGranted) {
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
        if (Utils.doLoggingFails(new String[] {"su", "exec " + logCommand})) {
          return null;
        }
      } else {
        Adb adbLogger;
        try {
          adbLogger = new Adb("exec " + logCommand);
        } catch (IOException e) {
          e.printStackTrace();
          return null;
        }
        Utils.runInBg(() -> Utils.readLogcatStream(null, adbLogger));
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
      readProcessLog(reader, DAEMON_CLASS_NAME);
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      Utils.cleanProcess(reader, process, adb, "readDaemonMessages");

      if (MySettings.getInstance().isPrivDaemonAlive()) {
        MySettings.getInstance().setPrivDaemonAlive(false);
        Log.e(TAG, "Privileged daemon died");
        SystemClock.sleep(5000);
        Log.i(TAG, "Restarting privileged daemon");
        startDaemon();
      }
    }
  }

  private void readProcessLog(BufferedReader reader, String tag) throws IOException {
    PrintWriter crashLogWriter = null;
    String line;
    while ((line = reader.readLine()) != null) {
      if (line.contains(Commands.CRASH_LOG_STARTS)) {
        File logFile = Utils.getCrashLogFile(true);
        if (logFile != null) {
          crashLogWriter = new PrintWriter(logFile);
          crashLogWriter.println(Utils.getDeviceInfo());
        }
        continue;
      }
      if (crashLogWriter != null) {
        crashLogWriter.println(line);
      }
      Log.e(tag, line);
    }
    if (crashLogWriter != null) {
      crashLogWriter.close();
    }
  }

  public Object sendRequest(String request) {
    synchronized (PrivDaemonHandler.class) {
      MySettings mySettings = MySettings.getInstance();
      if (!mySettings.isPrivDaemonAlive()) {
        Log.e(TAG, request + ": Privileged daemon is dead");
        return null;
      }

      if (cmdWriter == null || responseInStream == null) {
        Log.e(TAG, "CommandWriter or ResponseReader is null");
        return null;
      }

      // to avoid getting restarted
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

        Log.e(TAG, "Restarting privileged daemon");
        cmdWriter.println(Commands.SHUTDOWN);

        return null;
      }
    }
  }
}
