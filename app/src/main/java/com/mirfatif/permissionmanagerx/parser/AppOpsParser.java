package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;
import static com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler.DAEMON_HANDLER;

import android.content.pm.PackageManager;
import android.util.Log;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.svc.DaemonCmdRcvSvc;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.PrivTasks;
import com.mirfatif.privtasks.PrivTasks.PrivTasksCallback;
import com.mirfatif.privtasks.Util;
import com.mirfatif.privtasks.hiddenapis.err.HiddenAPIsError;
import com.mirfatif.privtasks.ser.MyPackageOps;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public enum AppOpsParser {
  APP_OPS_PARSER;

  private static final String TAG = "AppOpsParser";

  private final PrivTasks mPrivTasks =
      new PrivTasks(
          new PrivTasksCallbackImpl(),
          BuildConfig.APPLICATION_ID,
          DaemonCmdRcvSvc.class.getName(),
          Utils.getUserId(),
          false);

  AppOpsParser() {
    if (!mPrivTasks.canUseIAppOpsService()) {
      hiddenAPIsNotWorking("Could not initialize IAppOpsService");
    }
  }

  List<MyPackageOps> getOpsForPackage(int uid, String packageName, Integer op) {
    String _op = op == null ? "null" : String.valueOf(op);
    if (SETTINGS.canUseHiddenAPIs()) {
      try {
        return mPrivTasks.getMyPackageOpsList(uid, packageName, _op);
      } catch (HiddenAPIsError e) {
        hiddenAPIsNotWorking(e.toString());
        return getOpsForPackage(uid, packageName, op);
      }
    } else if (!SETTINGS.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": getOpsForPackage");
      return null;
    } else {
      List<MyPackageOps> list = new ArrayList<>();
      String request = Commands.GET_OPS_FOR_PKG_OR_UID + " " + uid + " " + packageName + " " + _op;
      Object object = DAEMON_HANDLER.sendRequest(request);

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

  private final List<String> mAppOpsList = new ArrayList<>();
  private final List<String> mAppOpsModes = new ArrayList<>();
  private final List<Integer> mOpToSwitchList = new ArrayList<>();
  private final List<Integer> mOpToDefModeList = new ArrayList<>();
  private final Map<String, Integer> mPermToOpCodeMap = new HashMap<>();

  public List<String> getAppOpsList() {
    return mAppOpsList;
  }

  public List<String> getAppOpsModes() {
    return mAppOpsModes;
  }

  List<Integer> getOpToSwitchList() {
    return mOpToSwitchList;
  }

  List<Integer> getOpToDefModeList() {
    return mOpToDefModeList;
  }

  Map<String, Integer> getPermToOpCodeMap() {
    return mPermToOpCodeMap;
  }

  private static final Object BUILD_LIST_LOCK = new Object();

  void buildAppOpsLists() {
    synchronized (BUILD_LIST_LOCK) {
      if (mAppOpsList.isEmpty()) {
        // Do not sort this list since the position is the AppOps code
        List<String> appOpsList = buildAppOpsList();
        if (appOpsList != null) {
          mAppOpsList.addAll(appOpsList);
        }
      }
      if (mAppOpsModes.isEmpty()) {
        List<String> appOpsModes = buildAppOpsModes();
        if (appOpsModes != null) {
          mAppOpsModes.addAll(appOpsModes);
        }
      }
      if (mOpToSwitchList.isEmpty()) {
        List<Integer> opToSwitchList = buildOpToSwitchList();
        if (opToSwitchList != null) {
          mOpToSwitchList.addAll(opToSwitchList);
        }
      }
      if (mOpToDefModeList.isEmpty()) {
        List<Integer> opToDefaultModeList = buildOpToDefaultModeList();
        if (opToDefaultModeList != null) {
          mOpToDefModeList.addAll(opToDefaultModeList);
        }
      }
      if (mPermToOpCodeMap.isEmpty()) {
        Map<String, Integer> permToOpCodeMap = buildPermissionToOpCodeMap();
        if (permToOpCodeMap != null) {
          mPermToOpCodeMap.putAll(permToOpCodeMap);
        }
      }
    }
  }

  private List<Integer> buildOpToDefaultModeList() {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "buildOpToDefaultModeList() called");
    }
    List<Integer> opToDefModeList = new ArrayList<>();
    if (SETTINGS.canUseHiddenAPIs()) {
      try {
        return mPrivTasks.buildOpToDefaultModeList();
      } catch (HiddenAPIsError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildOpToDefaultModeList();
      }
    } else if (!SETTINGS.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildOpToDefaultModeList");
      return null;
    } else {
      Object object = DAEMON_HANDLER.sendRequest(Commands.OP_TO_DEF_MODE_LIST);
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

  private List<Integer> buildOpToSwitchList() {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "buildOpToSwitchList() called");
    }
    List<Integer> opToSwitchList = new ArrayList<>();
    if (SETTINGS.canUseHiddenAPIs()) {
      try {
        return mPrivTasks.buildOpToSwitchList();
      } catch (HiddenAPIsError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildOpToSwitchList();
      }
    } else if (!SETTINGS.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildOpToSwitchList");
      return null;
    } else {
      Object object = DAEMON_HANDLER.sendRequest(Commands.OP_TO_SWITCH_LIST);
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

  private Map<String, Integer> buildPermissionToOpCodeMap() {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "buildPermissionToOpCodeMap() called");
    }
    Map<String, Integer> permToOpCodeMap = new HashMap<>();
    // IPackageManager returns bigger permissions list than PackageManager
    if (SETTINGS.canUseHiddenAPIs() && (mPrivTasks.canUseIPm() || !SETTINGS.isPrivDaemonAlive())) {
      try {
        PackageManager pm = mPrivTasks.canUseIPm() ? null : App.getContext().getPackageManager();
        for (String item : mPrivTasks.buildPermToOpCodeList(pm)) {
          String[] keyValue = item.split(":");
          permToOpCodeMap.put(keyValue[0], Integer.parseInt(keyValue[1]));
        }
        return permToOpCodeMap;
      } catch (HiddenAPIsError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildPermissionToOpCodeMap();
      }
    } else if (!SETTINGS.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildPermissionToOpCodeMap");
      return null;
    } else {
      Object object = DAEMON_HANDLER.sendRequest(Commands.PERM_TO_OP_CODE_LIST);
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

  private List<String> buildAppOpsList() {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "buildAppOpsList() called");
    }
    if (SETTINGS.excludeAppOpsPerms() || !SETTINGS.canReadAppOps()) {
      return null;
    }

    if (SETTINGS.canUseHiddenAPIs()) {
      try {
        return mPrivTasks.buildOpToNameList();
      } catch (HiddenAPIsError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildAppOpsList();
      }
    } else if (!SETTINGS.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildAppOpsList");
      return null;
    } else {
      Object object = DAEMON_HANDLER.sendRequest(Commands.OP_TO_NAME);
      if (object instanceof List<?>) {
        List<String> appOpsList = new ArrayList<>();
        for (Object item : (List<?>) object) {
          appOpsList.add((String) item);
        }
        return appOpsList;
      }
    }
    Log.e(TAG, "Error occurred in buildAppOpsList()");
    return null;
  }

  private List<String> buildAppOpsModes() {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "buildAppOpsModes() called");
    }
    if (SETTINGS.excludeAppOpsPerms() || !SETTINGS.canReadAppOps()) {
      return null;
    }
    List<String> appOpsModes = new ArrayList<>();

    if (SETTINGS.canUseHiddenAPIs()) {
      try {
        for (Object item : mPrivTasks.buildModeToNameList()) {
          appOpsModes.add(Utils.capitalizeString((String) item));
        }
        return appOpsModes;
      } catch (HiddenAPIsError e) {
        hiddenAPIsNotWorking(e.toString());
        return buildAppOpsModes();
      }
    } else if (!SETTINGS.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": buildAppOpsModes");
      return null;
    } else {
      Object object = DAEMON_HANDLER.sendRequest(Commands.MODE_TO_NAME);
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

  private void hiddenAPIsNotWorking(String error) {
    if (SETTINGS.useHiddenAPIs()) {
      Utils.showToast(R.string.hidden_apis_warning);
      SETTINGS.setUseHiddenAPIs(false);
    }
    Log.e(TAG, error);
  }

  private static class PrivTasksCallbackImpl implements PrivTasksCallback {

    @Override
    public boolean isDebug() {
      return SETTINGS.isDebug();
    }

    @Override
    public void logE(String msg) {
      Log.e(TAG, msg);
    }

    @Override
    public void sendRequest(String command) {
      DaemonCmdRcvSvc.showDaemonMsg(command);
    }
  }
}
