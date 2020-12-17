package com.mirfatif.privtasks;

public class Commands {
  private Commands() {}

  // the last String response, afterwards only serialized Objects are sent over stream
  public static final String HELLO = "HELLO";

  // listen over TCP socket
  public static final String CREATE_SOCKET = "CREATE_SOCKET";

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
}
