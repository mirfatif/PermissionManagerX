package com.mirfatif.privdaemon;

import android.os.Debug;
import android.os.Environment;
import android.os.Process;
import android.util.Log;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.PrivTasks;
import com.mirfatif.privtasks.PrivTasks.PrivTasksCallback;
import com.mirfatif.privtasks.Util;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.Inet4Address;
import java.net.ServerSocket;
import java.net.Socket;

public class PrivDaemon {

  private static String TAG = "com.mirfatif.privdaemon";

  private PrivTasks mPrivTasks;
  private PrivDaemonFlavor mPrivDaemonFlavor;
  private OutputStream mOutputStream;
  private InputStream mInputStream;
  private BufferedReader mCmdReader;
  private boolean DEBUG;
  private String mCodeWord;
  private String mAppId;

  private PrivDaemon() {
    setDefaultExceptionHandler();

    try (BufferedReader reader = new BufferedReader(new FileReader("/proc/self/cmdline"))) {
      TAG = reader.readLine().split("\0")[0];
    } catch (IOException e) {
      e.printStackTrace();
      return;
    }

    Log.i(TAG, "Reading parameters");
    String[] params;
    try {
      params = new BufferedReader(new InputStreamReader(System.in)).readLine().split(" ");
    } catch (IOException e) {
      e.printStackTrace();
      return;
    }

    DEBUG = Boolean.parseBoolean(params[0]);
    boolean useSocket = Boolean.parseBoolean(params[1]);
    int appUserId = Integer.parseInt(params[2]);
    mAppId = params[3];
    mCodeWord = params[4];

    mPrivTasks = new PrivTasks(new PrivTasksCallbackImpl(), mAppId, appUserId, true);
    mPrivDaemonFlavor = new PrivDaemonFlavor(this, mPrivTasks);

    for (int pid : mPrivTasks.getPidsForCommands(new String[] {TAG})) {
      if (pid != Process.myPid()) {
        Log.i(TAG, "Killing pid " + pid);
        Process.killProcess(pid);
      }
    }

    ServerSocket server = null;
    Socket client = null;
    int port = 0;
    if (useSocket) {
      if (DEBUG) {
        Log.d(TAG, "Creating server socket");
      }
      try {
        server = new ServerSocket(0, 0, Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}));
        port = server.getLocalPort();
        if (DEBUG) {
          Log.d(TAG, "Listening at port " + port);
        }
      } catch (IOException e) {
        mPrivTasks.sendRequest(Commands.LISTEN_ON_LOOPBACK_FAILED, mCodeWord);
        e.printStackTrace();
        return;
      }
    }

    // signal the client I'm up
    System.out.println(Commands.HELLO + ":" + Process.myPid() + ":" + Process.myUid() + ":" + port);
    System.out.flush();

    if (server == null) {
      mOutputStream = System.out;
      mInputStream = System.in;
    } else {
      try {
        Log.i(TAG, "Waiting for connection");
        client = server.accept();
        if (DEBUG) {
          Log.d(TAG, "Connection from " + client.getInetAddress() + ":" + client.getPort());
        }

        client.setTcpNoDelay(true);
        mOutputStream = client.getOutputStream();
        mInputStream = client.getInputStream();
      } catch (IOException e) {
        mPrivTasks.sendRequest(Commands.ESTABLISH_CONNECTION_FAILED, mCodeWord);
        e.printStackTrace();
        return;
      }
    }

    mCmdReader = new BufferedReader(new InputStreamReader(System.in));
    Log.i(TAG, "I'm up! Send commands (PID: " + Process.myPid() + ")");
    String line;
    try {
      // stop listening when app process is killed
      while ((line = mCmdReader.readLine()) != null) {
        if (DEBUG) {
          Util.debugLog(TAG, "Received command: " + line);
        }
        /**
         * trim() is required; AdbLib, or more precisely {@link
         * com.cgutman.adblib.AdbProtocol#generateMessage(int, int, int, byte[])}, adds some garbage
         * with write().
         */
        String[] args = line.trim().replaceAll("  +", " ").split(" ");
        if (args[0].equals(Commands.SHUTDOWN)) {
          break;
        } else {
          handleCommand(args);
        }
      }
      if (client != null) {
        if (DEBUG) {
          Log.d(TAG, "Closing client socket");
        }
        client.close();
      }
      if (server != null) {
        if (DEBUG) {
          Log.d(TAG, "Closing server socket");
        }
        server.close();
      }
      Log.i(TAG, "Bye bye! (PID: " + Process.myPid() + ")");
    } catch (IOException e) {
      System.err.println("Read/write error, shutting down");
      e.printStackTrace();
    }
    mPrivDaemonFlavor.onExit();
  }

  /////////////////////////////////////////////////////////////////////////////////

  private UncaughtExceptionHandler defaultExceptionHandler;

  private void setDefaultExceptionHandler() {
    defaultExceptionHandler = Thread.getDefaultUncaughtExceptionHandler();
    Thread.setDefaultUncaughtExceptionHandler(
        (t, e) -> {
          // Write log to file
          System.err.println(Commands.CRASH_LOG_STARTS);
          e.printStackTrace();
          defaultExceptionHandler.uncaughtException(t, e);
        });
  }

  private void dumpHeap() {
    String filesDir = "Android/data/" + mAppId + "/files";
    File dir = new File(Environment.getExternalStorageDirectory(), filesDir);
    if (dir.isDirectory()) {
      File file = new File(dir, "com.mirfatif.privdaemon.pmx.hprof");
      try {
        // "am dumpheap" does not work for non-app processes.
        Debug.dumpHprofData(file.getAbsolutePath());
      } catch (IOException ex) {
        ex.printStackTrace();
      }
    }
  }

  /*
   Use reset() to avoid building handle table in OOS and OIS which may cause OOM.
   Despite of using writeUnshared(), still null is written to handle table.
   https://courses.cs.washington.edu/courses/cse341/98au/java/jdk1.2beta4/docs/guide/serialization/serialfaq.html#OutOfMemoryError
  */

  public void resetOos() {
    if (mStdOutStream != null) {
      try {
        mStdOutStream.reset();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////////

  private void handleCommand(String[] args) throws IOException {
    switch (args[0]) {
      case Commands.GET_READY:
        mStdOutStream = new ObjectOutputStream(mOutputStream);
        mCmdReader = new BufferedReader(new InputStreamReader(mInputStream));
        sendResponse(Commands.GET_READY);
        break;
      case Commands.STOP_LOGGING:
        DEBUG = false;
        Log.i(TAG, "Please " + Commands.STOP_LOGGING);
        sendResponse(null);
        break;
      case Commands.GET_UID:
        sendResponse(Process.myUid());
        break;
      case Commands.OP_TO_NAME:
        sendResponse(mPrivTasks.buildOpToNameList());
        break;
      case Commands.MODE_TO_NAME:
        sendResponse(mPrivTasks.buildModeToNameList());
        break;
      case Commands.GET_OPS_FOR_PKG_OR_UID:
        sendResponse(mPrivTasks.getOpsForPackage(args));
        break;
      case Commands.OP_TO_DEF_MODE_LIST:
        sendResponse(mPrivTasks.buildOpToDefaultModeList());
        break;
      case Commands.OP_TO_SWITCH_LIST:
        sendResponse(mPrivTasks.buildOpToSwitchList());
        break;
      case Commands.PERM_TO_OP_CODE_LIST:
        sendResponse(mPrivTasks.buildPermToOpCodeList(null));
        break;
      case Commands.GRANT_PERMISSION:
        mPrivTasks.grantRevokePermission(true, args);
        sendResponse(null);
        break;
      case Commands.REVOKE_PERMISSION:
        mPrivTasks.grantRevokePermission(false, args);
        sendResponse(null);
        break;
      case Commands.ENABLE_PACKAGE:
        mPrivTasks.setAppEnabledState(true, args);
        sendResponse(null);
        break;
      case Commands.DISABLE_PACKAGE:
        mPrivTasks.setAppEnabledState(false, args);
        sendResponse(null);
        break;
      case Commands.SET_APP_OPS_MODE:
        mPrivTasks.setAppOpsMode(args);
        sendResponse(null);
        break;
      case Commands.RESET_APP_OPS:
        mPrivTasks.resetAppOps(args);
        sendResponse(null);
        break;
      case Commands.GET_OP_NUM:
        sendResponse(mPrivTasks.getNumOps());
        break;
      case Commands.GET_SYSTEM_FIXED_FLAG:
        sendResponse(HiddenAPIs.getSystemFixedFlag());
        break;
      case Commands.GET_POLICY_FIXED_FLAG:
        sendResponse(HiddenAPIs.getPolicyFixedFlag());
        break;
      case Commands.GET_PERMISSION_FLAGS:
        sendResponse(mPrivTasks.getPermissionFlags(args));
        break;
      case Commands.OPEN_APP_INFO:
        mPrivTasks.openAppInfo(args);
        sendResponse(null);
        break;
      case Commands.GET_PERM_STATUS:
        sendResponse(mPrivTasks.getPermStatus());
        break;
      case Commands.GET_APP_OP_STATUS:
        sendResponse(mPrivTasks.getAppOpsStatus());
        break;
      case Commands.RESET_OOS:
        resetOos();
        sendResponse(null);
        break;
      case Commands.DUMP_HEAP:
        dumpHeap();
        sendResponse(null);
        break;
      default:
        if (!mPrivDaemonFlavor.handleCommand(args)) {
          System.err.println("Unknown command: " + args[0]);
        }
    }
  }

  private ObjectOutputStream mStdOutStream;

  synchronized void sendResponse(Object object) {
    if (mStdOutStream == null) {
      return;
    }
    try {
      mStdOutStream.writeUnshared(object);
      mStdOutStream.flush();
    } catch (IOException e) {
      System.err.println("sendResponse: write error, shutting down");
      e.printStackTrace();
      System.exit(0);
    }
  }

  private class PrivTasksCallbackImpl implements PrivTasksCallback {

    @Override
    public boolean isDebug() {
      return DEBUG;
    }

    @Override
    public void logE(String msg) {
      System.err.println(msg);
    }

    @Override
    public void sendRequest(String command) {
      mPrivTasks.sendRequest(command, mCodeWord);
    }
  }

  public static void main(String[] args) {
    new PrivDaemon();
  }
}
