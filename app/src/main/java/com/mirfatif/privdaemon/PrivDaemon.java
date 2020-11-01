package com.mirfatif.privdaemon;

import android.app.AppOpsManager;
import android.content.Context;
import android.content.pm.IPackageManager;
import android.content.pm.PackageManager;
import android.content.pm.PermissionGroupInfo;
import android.content.pm.PermissionInfo;
import android.os.Build;
import android.os.Process;
import android.os.RemoteException;
import android.os.ServiceManager;
import android.util.Log;
import com.android.internal.app.IAppOpsService;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.net.Inet4Address;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PrivDaemon {

  public static String MY_NAME = "com.mirfatif.privdaemon";
  public static final String CLASS_NAME = "PrivDaemon";

  private final IAppOpsService mAppOpsService;
  private final IPackageManager mPackageManager;

  private PrivDaemon() {
    // asInterface() and getService() hidden APIs
    mAppOpsService =
        IAppOpsService.Stub.asInterface(ServiceManager.getService(Context.APP_OPS_SERVICE));
    // asInterface() and getService() hidden APIs
    mPackageManager = IPackageManager.Stub.asInterface(ServiceManager.getService("package"));
  }

  private void opToName() {
    List<String> appOpsList = new ArrayList<>();
    for (int i = 0; i < getOpNum(); i++) {
      // hidden API
      appOpsList.add(AppOpsManager.opToName(i));
    }
    sendResponse(appOpsList);
  }

  public static List<String> modeToName() {
    List<String> appOpsModes = new ArrayList<>();
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      // MODE_NAMES hidden API
      for (int i = 0; i < AppOpsManager.MODE_NAMES.length; i++) {
        // hidden API
        appOpsModes.add(AppOpsManager.modeToName(i));
      }
    } else {
      appOpsModes = Arrays.asList("allow", "ignore", "deny", "default");
    }
    return appOpsModes;
  }

  private void getOpsForPackage(String[] args) {
    int uid = Integer.parseInt(args[1]);
    try {
      sendResponse(getMyPackageOpsList(mAppOpsService, uid, args[2], args[3]));
    } catch (RemoteException e) {
      rateLimitThrowable(e);
      sendResponse(-1);
    }
  }

  public static List<MyPackageOps> getMyPackageOpsList(
      IAppOpsService appOpsService, int uid, String packageName, String op)
      throws RemoteException, NoSuchMethodError {
    int[] ops = op.equals("null") ? null : new int[] {Integer.parseInt(op)};
    List<AppOpsManager.PackageOps> pkgOpsList = null;

    if (!packageName.equals("null")) {
      // hidden API
      pkgOpsList = appOpsService.getOpsForPackage(uid, packageName, ops);
    } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      try {
        // hidden API
        pkgOpsList = appOpsService.getUidOps(uid, ops);
      } catch (NullPointerException e) {
        // Hey Android! You are buggy.
        if (DEBUG) e.printStackTrace();
        return new ArrayList<>();
      }
    }

    if (pkgOpsList == null) return new ArrayList<>();

    List<MyPackageOps> myPackageOpsList = new ArrayList<>();
    for (AppOpsManager.PackageOps packageOps : pkgOpsList) {
      MyPackageOps myPackageOps = new MyPackageOps();
      List<MyPackageOps.MyOpEntry> myOpEntryList = new ArrayList<>();

      for (AppOpsManager.OpEntry opEntry : packageOps.getOps()) {
        MyPackageOps.MyOpEntry myOpEntry = new MyPackageOps.MyOpEntry();

        // hidden API
        myOpEntry.op = opEntry.getOp();

        // MIUI returns 10005 op
        if (myOpEntry.op >= getOpNum()) {
          rateLimitLog(
              "getMyPackageOpsList()",
              "Bad op: " + myOpEntry.op + " for package: " + packageOps.getPackageName(),
              true);
          continue;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
          myOpEntry.lastAccessTime = opEntry.getLastAccessTime(AppOpsManager.OP_FLAGS_ALL);
        } else {
          // hidden API
          myOpEntry.lastAccessTime = opEntry.getTime();
        }
        myOpEntry.opMode = opEntry.getMode();

        myOpEntryList.add(myOpEntry);
      }

      myPackageOps.packageName = packageOps.getPackageName();
      myPackageOps.myOpEntryList = myOpEntryList;

      myPackageOpsList.add(myPackageOps);
    }
    return myPackageOpsList;
  }

  private void buildOpToDefaultModeList() {
    List<Integer> opToDefModeList = new ArrayList<>();
    for (int i = 0; i < getOpNum(); i++) {
      // hidden API
      opToDefModeList.add(AppOpsManager.opToDefaultMode(i));
    }
    sendResponse(opToDefModeList);
  }

  private void buildOpToSwitchList() {
    List<Integer> opToSwitchList = new ArrayList<>();
    for (int i = 0; i < getOpNum(); i++) {
      // hidden API
      opToSwitchList.add(AppOpsManager.opToSwitch(i));
    }
    sendResponse(opToSwitchList);
  }

  private static Integer NUM_OP = null;

  private static Integer getOpNum() {
    if (NUM_OP == null) {
      try {
        // hidden API
        Field idField = AppOpsManager.class.getDeclaredField("_NUM_OP");
        NUM_OP = idField.getInt(idField);
      } catch (NoSuchFieldException | IllegalAccessException e) {
        rateLimitThrowable(e);
        return 0;
      }
    }
    return NUM_OP;
  }

  private Integer getSystemFixedFlag() {
    try {
      // hidden API
      Field idField = PackageManager.class.getDeclaredField("FLAG_PERMISSION_SYSTEM_FIXED");
      return idField.getInt(idField);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      rateLimitThrowable(e);
      return -1;
    }
  }

  private Integer getPermissionFlags(String[] args) {
    try {
      // hidden API
      /**
       * requires system permissions: {@link PackageManager#getPermissionFlags(String, String,
       * UserHandle)}
       */
      return mPackageManager.getPermissionFlags(args[1], args[2], Integer.parseInt(args[3]));
    } catch (RemoteException | SecurityException e) {
      rateLimitThrowable(e);
      return -1;
    }
  }

  public static List<String> buildPermToOpCodeList(PackageManager pm, IPackageManager Ipm) {
    List<String> permToOpCodeList = new ArrayList<>();
    List<String> permGroupsList = new ArrayList<>();
    if (pm != null) {
      for (PermissionGroupInfo pgi : pm.getAllPermissionGroups(0)) {
        permGroupsList.add(pgi.name);
      }
    } else {
      try {
        // getAllPermissionGroups() hidden API
        for (Object pgi : Ipm.getAllPermissionGroups(0).getList()) {
          permGroupsList.add(((PermissionGroupInfo) pgi).name);
        }
      } catch (RemoteException e) {
        e.printStackTrace();
      }
    }
    permGroupsList.add(null);

    List<PermissionInfo> permInfoList = new ArrayList<>();
    for (String permGroup : permGroupsList) {
      if (pm != null) {
        try {
          permInfoList = pm.queryPermissionsByGroup(permGroup, 0);
        } catch (PackageManager.NameNotFoundException e) {
          rateLimitLog("buildPermToOpCodeList", e.toString(), false);
        }
      } else {
        try {
          // queryPermissionsByGroup() hidden API
          for (Object object : Ipm.queryPermissionsByGroup(permGroup, 0).getList()) {
            permInfoList.add((PermissionInfo) object);
          }
        } catch (RemoteException e) {
          e.printStackTrace();
        }
      }
    }

    for (PermissionInfo permInfo : permInfoList) {
      if (!permInfo.packageName.equals("android")) continue;
      // hidden API
      int opCode = AppOpsManager.permissionToOpCode(permInfo.name);
      if (opCode != AppOpsManager.OP_NONE) {
        permToOpCodeList.add(permInfo.name + ":" + opCode);
      }
    }
    return permToOpCodeList;
  }

  private void grantRevokePermission(boolean grant, String[] args) {
    try {
      if (grant) {
        // hidden API
        // requires android.permission.GRANT_RUNTIME_PERMISSIONS
        mPackageManager.grantRuntimePermission(args[1], args[2], Integer.parseInt(args[3]));
      } else {
        // hidden API
        // requires android.permission.REVOKE_RUNTIME_PERMISSIONS
        mPackageManager.revokeRuntimePermission(args[1], args[2], Integer.parseInt(args[3]));
      }
      sendResponse(null);
    } catch (RemoteException | SecurityException e) {
      // SecurityException is thrown if calling UID/PID doesn't have required permissions
      // e.g. ADB lacks these permissions on MIUI
      e.printStackTrace();
      sendResponse(-1);
    }
  }

  private void setAppOpsMode(String[] args) {
    // hidden API
    int op = AppOpsManager.strDebugOpToOp(args[1]);
    int uid = Integer.parseInt(args[2]);
    String pkgName = args[3];
    int mode = Integer.parseInt(args[4]);

    try {
      if (pkgName.equals("null")) {
        mAppOpsService.setUidMode(op, uid, mode);
      } else {
        // requires android.permission.UPDATE_APP_OPS_STATS
        mAppOpsService.setMode(op, uid, pkgName, mode);
      }
      sendResponse(null);
    } catch (RemoteException | SecurityException e) {
      e.printStackTrace();
      sendResponse(-1);
    }
  }

  private void resetAppOps(String[] args) {
    try {
      // hidden API
      // requires android.permission.UPDATE_APP_OPS_STATS
      mAppOpsService.resetAllModes(Integer.parseInt(args[1]), args[2]);
      sendResponse(null);
    } catch (RemoteException | SecurityException e) {
      e.printStackTrace();
      sendResponse(-1);
    }
  }

  private void setEnabledState(boolean enabled, String[] args) {
    String pkg = args[1];
    int userid = Integer.parseInt(args[2]);
    String callingPkg = "shell:" + Process.myUid();

    int state;
    if (enabled) {
      state = PackageManager.COMPONENT_ENABLED_STATE_ENABLED;
    } else {
      state = PackageManager.COMPONENT_ENABLED_STATE_DISABLED_USER;
    }

    try {
      // hidden API
      mPackageManager.setApplicationEnabledSetting(pkg, state, 0, userid, callingPkg);
      sendResponse(null);
    } catch (RemoteException e) {
      e.printStackTrace();
      sendResponse(-1);
    }
  }

  private ObjectOutputStream mStdOutStream;

  private void sendResponse(Object object) {
    try {
      mStdOutStream.writeObject(object);
      mStdOutStream.flush();
    } catch (IOException e) {
      e.printStackTrace();
      Log.e(MY_NAME, "Write error, shutting down");
      System.exit(0);
    }
  }

  private static long lastThrowableTimestamp = 0;

  private static void rateLimitThrowable(Throwable t) {
    if (!DEBUG && System.currentTimeMillis() - lastThrowableTimestamp < 1000) {
      return;
    }
    t.printStackTrace();
    lastThrowableTimestamp = System.currentTimeMillis();
  }

  private static long lastLogTimestamp = 0;

  private static void rateLimitLog(String tag, String msg, boolean isError) {
    if (DEBUG) {
      if (isError) Log.e(tag, msg + " - " + System.nanoTime());
      else Log.i(tag, msg + " - " + System.nanoTime());
    } else if (System.currentTimeMillis() - lastLogTimestamp >= 1000) {
      if (isError) Log.e(tag, msg);
      else Log.i(tag, msg);
      lastLogTimestamp = System.currentTimeMillis();
    }
  }

  private void debugLog(String message) {
    Log.d(MY_NAME, message + " - " + System.nanoTime());
  }

  // verbose logging
  private static boolean DEBUG = false;

  // the last String response, afterwards only serialized Objects are sent over stream
  public static final String HELLO = "HELLO";

  // listen over TCP socket
  public static final String CREATE_SOCKET = "CREATE_SOCKET";

  // commands
  public static final String GET_READY = "GET_READY";
  public static final String STOP_LOGGING = "STOP_LOGGING";
  public static final String SHUTDOWN = "SHUTDOWN";
  public static final String OP_TO_NAME = "OP_TO_NAME";
  public static final String MODE_TO_NAME = "MODE_TO_NAME";
  public static final String GET_OPS_FOR_PKG_OR_UID = "GET_OPS_FOR_PKG_OR_UID";
  public static final String OP_TO_DEF_MODE_LIST = "OP_TO_DEF_MODE_LIST";
  public static final String OP_TO_SWITCH_LIST = "OP_TO_SWITCH_LIST";
  public static final String PERM_TO_OP_CODE_LIST = "PERM_TO_OP_CODE_LIST";
  public static final String GRANT_PERMISSION = "GRANT_PERMISSION";
  public static final String REVOKE_PERMISSION = "REVOKE_PERMISSION";
  public static final String ENABLE_PACKAGE = "ENABLE_PACKAGE";
  public static final String DISABLE_PACKAGE = "DISABLE_PACKAGE";
  public static final String SET_APP_OPS_MODE = "SET_APP_OPS_MODE";
  public static final String RESET_APP_OPS = "RESET_APP_OPS";
  public static final String GET_OP_NUM = "GET_OP_NUM";
  public static final String GET_SYSTEM_FIXED_FLAG = "GET_SYSTEM_FIXED_FLAG";
  public static final String GET_PERMISSION_FLAGS = "GET_PERMISSION_FLAGS";

  public static void main(String[] arguments) {

    try (BufferedReader reader = new BufferedReader(new FileReader("/proc/self/cmdline"))) {
      MY_NAME = reader.readLine().split("\0")[0];
    } catch (IOException e) {
      e.printStackTrace();
      return;
    }

    DEBUG = Boolean.parseBoolean(arguments[0]);

    // hidden API
    for (int pid : Process.getPidsForCommands(new String[] {MY_NAME})) {
      if (pid != Process.myPid()) {
        Log.i(MY_NAME, "Killing pid " + pid);
        Process.killProcess(pid);
      }
    }

    ServerSocket server = null;
    Socket client = null;
    int port = 0;
    if (Arrays.asList(arguments).contains(CREATE_SOCKET)) {
      if (DEBUG) Log.d(MY_NAME, "Creating server socket");
      try {
        server = new ServerSocket(0, 0, Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}));
        port = server.getLocalPort();
        if (DEBUG) Log.d(MY_NAME, "Listening at port " + port);
      } catch (IOException e) {
        e.printStackTrace();
        return;
      }
    }

    // signal the client I'm up
    System.out.println(HELLO + ":" + Process.myPid() + ":" + port);
    System.out.flush();

    OutputStream outputStream;
    InputStream inputStream;
    if (server == null) {
      outputStream = System.out;
      inputStream = System.in;
    } else {
      try {
        Log.i(MY_NAME, "Waiting for connection");
        client = server.accept();
        if (DEBUG) {
          Log.d(MY_NAME, "Connection from " + client.getInetAddress() + ":" + client.getPort());
        }

        client.setTcpNoDelay(true);
        outputStream = client.getOutputStream();
        inputStream = client.getInputStream();
      } catch (IOException e) {
        e.printStackTrace();
        return;
      }
    }

    PrivDaemon mPrivDaemon = new PrivDaemon();
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    Log.i(MY_NAME, "I'm up! Send commands");
    String line;
    try {
      // stop listening when app process is killed
      readCommandLoop:
      while ((line = reader.readLine()) != null) {
        if (DEBUG) mPrivDaemon.debugLog("Received command: " + line);
        /**
         * trim() is required; AdbLib, or more precisely {@link
         * com.cgutman.adblib.AdbProtocol#generateMessage(int, int, int, byte[])}, adds some garbage
         * with write().
         */
        String[] args = line.trim().split(" ");
        switch (args[0]) {
          case GET_READY:
            mPrivDaemon.mStdOutStream = new ObjectOutputStream(outputStream);
            reader = new BufferedReader(new InputStreamReader(inputStream));
            mPrivDaemon.sendResponse(GET_READY);
            break;
          case STOP_LOGGING:
            DEBUG = false;
            Log.i(MY_NAME, STOP_LOGGING);
            mPrivDaemon.sendResponse(null);
            break;
          case OP_TO_NAME:
            mPrivDaemon.opToName();
            break;
          case MODE_TO_NAME:
            mPrivDaemon.sendResponse(PrivDaemon.modeToName());
            break;
          case GET_OPS_FOR_PKG_OR_UID:
            mPrivDaemon.getOpsForPackage(args);
            break;
          case OP_TO_DEF_MODE_LIST:
            mPrivDaemon.buildOpToDefaultModeList();
            break;
          case OP_TO_SWITCH_LIST:
            mPrivDaemon.buildOpToSwitchList();
            break;
          case PERM_TO_OP_CODE_LIST:
            mPrivDaemon.sendResponse(buildPermToOpCodeList(null, mPrivDaemon.mPackageManager));
            break;
          case GRANT_PERMISSION:
            mPrivDaemon.grantRevokePermission(true, args);
            break;
          case REVOKE_PERMISSION:
            mPrivDaemon.grantRevokePermission(false, args);
            break;
          case ENABLE_PACKAGE:
            mPrivDaemon.setEnabledState(true, args);
            break;
          case DISABLE_PACKAGE:
            mPrivDaemon.setEnabledState(false, args);
            break;
          case SET_APP_OPS_MODE:
            mPrivDaemon.setAppOpsMode(args);
            break;
          case RESET_APP_OPS:
            mPrivDaemon.resetAppOps(args);
            break;
          case GET_OP_NUM:
            mPrivDaemon.sendResponse(PrivDaemon.getOpNum());
            break;
          case GET_SYSTEM_FIXED_FLAG:
            mPrivDaemon.sendResponse(mPrivDaemon.getSystemFixedFlag());
            break;
          case GET_PERMISSION_FLAGS:
            mPrivDaemon.sendResponse(mPrivDaemon.getPermissionFlags(args));
            break;
          case SHUTDOWN:
            break readCommandLoop;
          default:
            Log.e(MY_NAME, "Unknown command: " + args[0]);
        }
      }
      if (client != null) {
        if (DEBUG) Log.d(MY_NAME, "Closing client socket");
        client.close();
      }
      if (server != null) {
        if (DEBUG) Log.d(MY_NAME, "Closing server socket");
        server.close();
      }
      Log.i(MY_NAME, "Bye bye!");
    } catch (IOException e) {
      e.printStackTrace();
      Log.e(MY_NAME, "Read error, shutting down");
    }
  }
}
