package com.mirfatif.permissionmanagerx.parser;

import android.app.AppOpsManager;
import android.content.pm.PackageManager;
import android.os.RemoteException;
import android.util.Log;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.MyPackageOps;
import com.mirfatif.privtasks.PrivTasks;
import com.mirfatif.privtasks.Util;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AppOpsParser {

  private static final String TAG = "AppOpsParser";

  private static AppOpsParser mAppOpsParser;

  public static AppOpsParser getInstance() {
    if (mAppOpsParser == null) {
      mAppOpsParser = new AppOpsParser();
    }
    return mAppOpsParser;
  }

  private final MySettings mMySettings;
  private final PrivDaemonHandler mPrivDaemonHandler;
  private final PrivTasks mPrivTasks;

  // IPackageManager returns bigger permissions list than PackageManager
  private boolean mUseIPackageManager = true;

  private AppOpsParser() {
    mMySettings = MySettings.getInstance();
    mPrivDaemonHandler = PrivDaemonHandler.getInstance();
    mPrivTasks = new PrivTasks(mMySettings.isDebug(), false);

    // asInterface() and getService() hidden APIs
    try {
      mPrivTasks.initializeAppOpsService();
    } catch (NoSuchMethodError e) {
      // Simple AppOpsManager doesn't have getUidOps()
      hiddenAPIsNotWorking(e.toString());
    }
    try {
      mPrivTasks.initializePmService();
    } catch (NoSuchMethodError e) {
      // We can use simple PackageManager
      mUseIPackageManager = false;
      Log.e(TAG, e.toString());
    }
  }

  List<MyPackageOps> getOpsForPackage(int uid, String packageName, Integer op) {
    String _op = op == null ? "null" : String.valueOf(op);
    if (mMySettings.canUseHiddenAPIs()) {
      try {
        return mPrivTasks.getMyPackageOpsList(uid, packageName, _op);
      } catch (RemoteException e) {
        Log.e(TAG, e.toString());
      } catch (NoSuchMethodError e) {
        hiddenAPIsNotWorking(e.toString());
        return getOpsForPackage(uid, packageName, op);
      }
    } else if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getOpsForPackage");
      return null;
    } else {
      List<MyPackageOps> list = new ArrayList<>();
      String request = Commands.GET_OPS_FOR_PKG_OR_UID + " " + uid + " " + packageName + " " + _op;
      Object object = mPrivDaemonHandler.sendRequest(request);

      if (object instanceof List<?>) {
        List<?> objectList = (List<?>) object;
        for (Object item : objectList) {
          list.add((MyPackageOps) item);
        }
        return list;
      }
    }
    String message = !packageName.equals("null") ? "getOpsForPackage()" : "getUidOps()";
    Log.e(TAG, "Error occurred in " + message);
    return null;
  }

  List<MyPackageOps> getUidOps(int uid) {
    return getOpsForPackage(uid, "null", null);
  }

  List<Integer> buildOpToDefaultModeList() {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "buildOpToDefaultModeList() called");
    }
    List<Integer> opToDefModeList = new ArrayList<>();
    if (mMySettings.canUseHiddenAPIs()) {
      try {
        for (int i = 0; i < getOpNum(); i++) {
          // hidden API
          opToDefModeList.add(AppOpsManager.opToDefaultMode(i));
        }
        return opToDefModeList;
      } catch (NoSuchMethodError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildOpToDefaultModeList();
      }
    } else if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildOpToDefaultModeList");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(Commands.OP_TO_DEF_MODE_LIST);
      if (object instanceof List<?>) {
        for (Object item : (List<?>) object) {
          opToDefModeList.add((Integer) item);
        }
        return opToDefModeList;
      }
    }
    Log.e(TAG, "Error occurred in buildOpToDefaultModeList()");
    return null;
  }

  List<Integer> buildOpToSwitchList() {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "buildOpToSwitchList() called");
    }
    List<Integer> opToSwitchList = new ArrayList<>();
    if (mMySettings.canUseHiddenAPIs()) {
      try {
        for (int i = 0; i < getOpNum(); i++) {
          // hidden API
          opToSwitchList.add(AppOpsManager.opToSwitch(i));
        }
        return opToSwitchList;
      } catch (NoSuchMethodError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildOpToSwitchList();
      }
    } else if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildOpToSwitchList");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(Commands.OP_TO_SWITCH_LIST);
      if (object instanceof List<?>) {
        for (Object item : (List<?>) object) {
          opToSwitchList.add((Integer) item);
        }
        return opToSwitchList;
      }
    }
    Log.e(TAG, "Error occurred in buildOpToSwitchList()");
    return null;
  }

  Map<String, Integer> buildPermissionToOpCodeMap() {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "buildPermissionToOpCodeMap() called");
    }
    Map<String, Integer> permToOpCodeMap = new HashMap<>();
    if (mMySettings.canUseHiddenAPIs()
        && (mUseIPackageManager || !mMySettings.isPrivDaemonAlive())) {
      try {
        PackageManager pm = mUseIPackageManager ? null : App.getContext().getPackageManager();
        for (String item : mPrivTasks.buildPermToOpCodeList(pm)) {
          String[] keyValue = item.split(":");
          permToOpCodeMap.put(keyValue[0], Integer.parseInt(keyValue[1]));
        }
        return permToOpCodeMap;
      } catch (NoSuchMethodError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildPermissionToOpCodeMap();
      }
    } else if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildPermissionToOpCodeMap");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(Commands.PERM_TO_OP_CODE_LIST);
      if (object instanceof List<?>) {
        for (Object item : (List<?>) object) {
          String[] keyValue = ((String) item).split(":");
          permToOpCodeMap.put(keyValue[0], Integer.parseInt(keyValue[1]));
        }
        return permToOpCodeMap;
      }
    }
    Log.e(TAG, "Error occurred in buildPermissionToOpCodeMap()");
    return null;
  }

  public List<String> buildAppOpsList() {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "buildAppOpsList() called");
    }
    if (mMySettings.excludeAppOpsPerms() || !mMySettings.canReadAppOps()) {
      return null;
    }
    List<String> appOpsList = new ArrayList<>();

    if (mMySettings.canUseHiddenAPIs()) {
      try {
        for (int i = 0; i < getOpNum(); i++) {
          // hidden API
          appOpsList.add(AppOpsManager.opToName(i));
        }
        return appOpsList;
      } catch (NoSuchMethodError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildAppOpsList();
      }
    } else if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildAppOpsList");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(Commands.OP_TO_NAME);
      if (object instanceof List<?>) {
        for (Object item : (List<?>) object) {
          appOpsList.add((String) item);
        }
        return appOpsList;
      }
    }
    Log.e(TAG, "Error occurred in buildAppOpsList()");
    return null;
  }

  public List<String> buildAppOpsModes() {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "buildAppOpsModes() called");
    }
    if (mMySettings.excludeAppOpsPerms() || !mMySettings.canReadAppOps()) {
      return null;
    }
    List<String> appOpsModes = new ArrayList<>();

    if (mMySettings.canUseHiddenAPIs()) {
      try {
        for (Object item : mPrivTasks.buildModeToNameList()) {
          appOpsModes.add(Utils.capitalizeString((String) item));
        }
        return appOpsModes;
      } catch (NoSuchMethodError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildAppOpsModes();
      }
    } else if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildAppOpsModes");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(Commands.MODE_TO_NAME);
      if (object instanceof List<?>) {
        for (Object item : (List<?>) object) {
          appOpsModes.add(Utils.capitalizeString((String) item));
        }
        return appOpsModes;
      }
    }
    Log.e(TAG, "Error occurred in buildAppOpsModes()");
    return null;
  }

  private int getOpNum() {
    if (mMySettings.canUseHiddenAPIs()) {
      /** {@link AppOpsManager#_NUM_OP} gets hard-coded */
      // hidden API
      Integer NUM_OP = Utils.getStaticIntField("_NUM_OP", AppOpsManager.class, TAG + " getOpNum()");
      if (NUM_OP != null) {
        return NUM_OP;
      }
      hiddenAPIsNotWorking("Could not get _NUM_OP field");
      return getOpNum();
    } else if (!mMySettings.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getOpNum");
      return 0;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(Commands.GET_OP_NUM);
      if (object instanceof Integer) {
        return (int) object;
      }
    }
    Log.e(TAG, "Error occurred in getOpNum()");
    return 0;
  }

  private void hiddenAPIsNotWorking(String error) {
    PackageParser.getInstance().hiddenAPIsNotWorking(TAG, error);
  }
}
