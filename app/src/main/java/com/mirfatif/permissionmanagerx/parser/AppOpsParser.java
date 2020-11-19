package com.mirfatif.permissionmanagerx.parser;

import android.app.AppOpsManager;
import android.content.Context;
import android.content.pm.PackageManager;
import android.os.IBinder;
import android.os.RemoteException;
import android.os.ServiceManager;
import android.util.Log;
import com.android.internal.app.IAppOpsService;
import com.mirfatif.permissionmanagerx.MySettings;
import com.mirfatif.permissionmanagerx.PrivDaemonHandler;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.privdaemon.MyPackageOps;
import com.mirfatif.privdaemon.PrivDaemon;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AppOpsParser {

  static final String TAG = "AppOpsParser";

  private final MySettings mMySettings;
  private final PrivDaemonHandler mPrivDaemonHandler;
  private final PackageManager mPackageManager;

  AppOpsParser(PackageManager packageManager) {
    mPackageManager = packageManager;
    mMySettings = MySettings.getInstance();
    mPrivDaemonHandler = PrivDaemonHandler.getInstance();
  }

  // AppOpsManager doesn't have getUidOps()
  private IAppOpsService mAppOpsService;

  List<MyPackageOps> getOpsForPackage(int uid, String packageName, Integer op) {
    String _op = op == null ? "null" : String.valueOf(op);
    if (mMySettings.useHiddenAPIs()) {
      try {
        if (mAppOpsService == null) {
          // getService() hidden API
          IBinder iBinderAppOps = ServiceManager.getService(Context.APP_OPS_SERVICE);
          // asInterface() hidden API
          mAppOpsService = IAppOpsService.Stub.asInterface(iBinderAppOps);
        }
        return PrivDaemon.getMyPackageOpsList(mAppOpsService, uid, packageName, _op);
      } catch (RemoteException e) {
        Log.e(TAG, e.toString());
      } catch (NoSuchMethodError e) {
        Utils.hiddenAPIsNotWorking(TAG, e.toString());
        return getOpsForPackage(uid, packageName, op);
      }
    } else if (!mMySettings.mPrivDaemonAlive) {
      Utils.logDaemonDead(TAG + ": getOpsForPackage");
      return null;
    } else {
      List<MyPackageOps> list = new ArrayList<>();
      String request =
          PrivDaemon.GET_OPS_FOR_PKG_OR_UID + " " + uid + " " + packageName + " " + _op;
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
    if (mMySettings.DEBUG) Utils.debugLog("PackageParser", "buildOpToDefaultModeList() called");
    List<Integer> opToDefModeList = new ArrayList<>();
    if (mMySettings.useHiddenAPIs()) {
      try {
        for (int i = 0; i < getOpNum(); i++) {
          // hidden API
          opToDefModeList.add(AppOpsManager.opToDefaultMode(i));
        }
        return opToDefModeList;
      } catch (NoSuchMethodError e) {
        Utils.hiddenAPIsNotWorking(TAG, e.toString());
        return buildOpToDefaultModeList();
      }
    } else if (!mMySettings.mPrivDaemonAlive) {
      Utils.logDaemonDead(TAG + ": buildOpToDefaultModeList");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(PrivDaemon.OP_TO_DEF_MODE_LIST);
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
    if (mMySettings.DEBUG) Utils.debugLog("PackageParser", "buildOpToSwitchList() called");
    List<Integer> opToSwitchList = new ArrayList<>();
    if (mMySettings.useHiddenAPIs()) {
      try {
        for (int i = 0; i < getOpNum(); i++) {
          // hidden API
          opToSwitchList.add(AppOpsManager.opToSwitch(i));
        }
        return opToSwitchList;
      } catch (NoSuchMethodError e) {
        Utils.hiddenAPIsNotWorking(TAG, e.toString());
        return buildOpToSwitchList();
      }
    } else if (!mMySettings.mPrivDaemonAlive) {
      Utils.logDaemonDead(TAG + ": buildOpToSwitchList");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(PrivDaemon.OP_TO_SWITCH_LIST);
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
    if (mMySettings.DEBUG) Utils.debugLog("PackageParser", "buildPermissionToOpCodeMap() called");
    Map<String, Integer> permToOpCodeMap = new HashMap<>();
    if (mMySettings.useHiddenAPIs()) {
      try {
        for (String item : PrivDaemon.buildPermToOpCodeList(mPackageManager, null)) {
          String[] keyValue = item.split(":");
          permToOpCodeMap.put(keyValue[0], Integer.parseInt(keyValue[1]));
        }
        return permToOpCodeMap;
      } catch (NoSuchMethodError e) {
        Utils.hiddenAPIsNotWorking(TAG, e.toString());
        return buildPermissionToOpCodeMap();
      }
    } else if (!mMySettings.mPrivDaemonAlive) {
      Utils.logDaemonDead(TAG + ": buildPermissionToOpCodeMap");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(PrivDaemon.PERM_TO_OP_CODE_LIST);
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

  List<String> buildAppOpsList() {
    if (mMySettings.DEBUG) Utils.debugLog("PackageParser", "buildAppOpsList() called");
    if (mMySettings.excludeAppOpsPerms() || !mMySettings.canReadAppOps()) return null;
    List<String> appOpsList = new ArrayList<>();

    if (mMySettings.useHiddenAPIs()) {
      try {
        for (int i = 0; i < getOpNum(); i++) {
          // hidden API
          appOpsList.add(AppOpsManager.opToName(i));
        }
        return appOpsList;
      } catch (NoSuchMethodError e) {
        Utils.hiddenAPIsNotWorking(TAG, e.toString());
        return buildAppOpsList();
      }
    } else if (!mMySettings.mPrivDaemonAlive) {
      Utils.logDaemonDead(TAG + ": buildAppOpsList");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(PrivDaemon.OP_TO_NAME);
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

  List<String> buildAppOpsModes() {
    if (mMySettings.DEBUG) Utils.debugLog("PackageParser", "buildAppOpsModes() called");
    if (mMySettings.excludeAppOpsPerms() || !mMySettings.canReadAppOps()) return null;
    List<String> appOpsModes = new ArrayList<>();

    if (mMySettings.useHiddenAPIs()) {
      try {
        for (Object item : PrivDaemon.modeToName()) {
          appOpsModes.add(Utils.capitalizeString((String) item));
        }
        return appOpsModes;
      } catch (NoSuchMethodError e) {
        Utils.hiddenAPIsNotWorking(TAG, e.toString());
        return buildAppOpsModes();
      }
    } else if (!mMySettings.mPrivDaemonAlive) {
      Utils.logDaemonDead(TAG + ": buildAppOpsModes");
      return null;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(PrivDaemon.MODE_TO_NAME);
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
    if (mMySettings.useHiddenAPIs()) {
      /** {@link AppOpsManager#_NUM_OP} gets hard-coded */
      // hidden API
      int NUM_OP = Utils.getIntField("_NUM_OP", AppOpsManager.class, TAG + " getOpNum()");
      if (NUM_OP != Utils.INT_FIELD_ERROR) return NUM_OP;
      Utils.hiddenAPIsNotWorking(TAG, "Could not get _NUM_OP field");
      return getOpNum();
    } else if (!mMySettings.mPrivDaemonAlive) {
      Utils.logDaemonDead(TAG + ": getOpNum");
      return 0;
    } else {
      Object object = mPrivDaemonHandler.sendRequest(PrivDaemon.GET_OP_NUM);
      if (object instanceof Integer) return (int) object;
    }
    Log.e(TAG, "Error occurred in getOpNum()");
    return 0;
  }
}
