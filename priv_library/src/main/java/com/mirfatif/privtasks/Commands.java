package com.mirfatif.privtasks;

public class Commands {

  private Commands() {}

  // the last String response, afterwards only serialized Objects are sent over stream
  public static final String HELLO = "HELLO";

  // listen over TCP socket
  public static final String CREATE_SOCKET = "CREATE_SOCKET";

  // Indicates the start of a crash log which the app handles
  public static final String CRASH_LOG_STARTS = "PRIVILEGED_DAEMON_CRASH_LOG_STARTS";

  public static final String GET_READY = "GET_READY";
  public static final String STOP_LOGGING = "STOP_LOGGING";
  public static final String SHUTDOWN = "SHUTDOWN";
  public static final String GET_UID = "GET_UID";
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
  public static final String GET_POLICY_FIXED_FLAG = "GET_POLICY_FIXED_FLAG";
  public static final String GET_PERMISSION_FLAGS = "GET_PERMISSION_FLAGS";
  public static final String GET_INSTALLED_PKGS = "GET_INSTALLED_PKGS";
  public static final String GET_PKG_INFO = "GET_PKG_INFO";
  public static final String OPEN_APP_INFO = "OPEN_APP_INFO";
  public static final String GET_USERS = "GET_USERS";
  public static final String SET_PERM_FLAGS = "SET_PERM_FLAGS";

  // From daemon to app
  public static final String CMD_RCV_SVC = "com.mirfatif.permissionmanagerx.svc.DaemonCmdRcvSvc";
  public static final String CODE_WORD = "CODE_WORD";

  public static final String LISTEN_ON_LOOPBACK_FAILED = "LISTEN_ON_LOOPBACK_FAILED";
  public static final String ESTABLISH_CONNECTION_FAILED = "ESTABLISH_CONNECTION_FAILED";

  public static final String WRONG_ARGS_RECEIVED = "WRONG_ARGS";
  public static final String SET_APP_OPS_MODE_FAILED = "SET_APP_OPS_MODE_FAILED";
  public static final String RESET_APP_OPS_FAILED = "RESET_APP_OPS_FAILED";
  public static final String GRANT_PERM_FAILED = "GRANT_PERM_FAILED";
  public static final String REVOKE_PERM_FAILED = "REVOKE_PERM_FAILED";
  public static final String ENABLE_PKG_FAILED = "ENABLE_PKG_FAILED";
  public static final String DISABLE_PKG_FAILED = "DISABLE_PKG_FAILED";
  public static final String OPEN_APP_INFO_FAILED = "OPEN_APP_INFO_FAILED";
  public static final String GET_PKG_INFO_FAILED = "GET_PKG_INFO_FAILED";
  public static final String GET_INSTALLED_PKGS_FAILED = "GET_INSTALLED_PKGS_FAILED";
  public static final String GET_USERS_FAILED = "GET_USERS_FAILED";
  public static final String GET_PERM_GRP_INFO_LIST_FAILED = "GET_PERM_GRP_INFO_LIST_FAILED";
  public static final String OP_NUM_INCONSISTENCY = "OP_NUM_INCONSISTENCY";
  public static final String OP_TO_DEF_MODE_NOT_FOUND = "OP_TO_DEF_MODE_NOT_FOUND";
  public static final String SET_PERM_FLAGS_FAILED = "SET_PERM_FLAGS_FAILED";
}
