package com.mirfatif.permissionmanagerx.svc;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import java.util.UUID;

public class DaemonCmdRcvSvc extends Service {

  private static final String TAG = "DaemonCmdRcvSvc";

  public static final String CODE_WORD = UUID.randomUUID().toString();

  @Override
  public IBinder onBind(Intent intent) {
    return null;
  }

  @Override
  public int onStartCommand(Intent intent, int flags, int startId) {
    String action = intent.getAction();
    if (action != null && CODE_WORD.equals(intent.getStringExtra(Commands.CODE_WORD))) {
      if (!DaemonCmdRcvSvcFlavor.handleCmd(action)) {
        Utils.runInBg(() -> showDaemonMsg(action));
      }
    }
    return Service.START_NOT_STICKY;
  }

  private static long mShowMsgTs;

  private static boolean shouldShowMsg() {
    boolean res = System.currentTimeMillis() - mShowMsgTs > 5000;
    if (res) {
      mShowMsgTs = System.currentTimeMillis();
    }
    return res;
  }

  public static void showDaemonMsg(String command) {
    if (!shouldShowMsg()) {
      return;
    }

    switch (command) {
      case Commands.LISTEN_ON_LOOPBACK_FAILED:
        Utils.showToast(R.string.daemon_listen_on_loopback_failed);
        break;
      case Commands.ESTABLISH_CONNECTION_FAILED:
        Utils.showToast(R.string.daemon_establish_connection_failed);
        break;
      case Commands.WRONG_ARGS_RECEIVED:
        Utils.showToast(R.string.daemon_wrong_args_received);
        break;
      case Commands.SET_APP_OPS_MODE_FAILED:
        Utils.showToast(R.string.daemon_set_app_ops_mode_failed);
        break;
      case Commands.RESET_APP_OPS_FAILED:
        Utils.showToast(R.string.daemon_reset_app_ops_failed);
        break;
      case Commands.GRANT_PERM_FAILED:
        Utils.showToast(R.string.daemon_grant_perm_failed);
        break;
      case Commands.REVOKE_PERM_FAILED:
        Utils.showToast(R.string.daemon_revoke_perm_failed);
        break;
      case Commands.ENABLE_PKG_FAILED:
        Utils.showToast(R.string.daemon_enable_pkg_failed);
        break;
      case Commands.DISABLE_PKG_FAILED:
        Utils.showToast(R.string.daemon_disable_pkg_failed);
        break;
      case Commands.OPEN_APP_INFO_FAILED:
        Utils.showToast(R.string.daemon_open_app_info_failed);
        break;
      case Commands.GET_PERM_GRP_INFO_LIST_FAILED:
        Utils.showToast(R.string.daemon_get_perm_grp_info_list_failed);
        break;
      case Commands.OP_NUM_INCONSISTENCY:
      case Commands.OP_TO_DEF_MODE_NOT_FOUND:
        Utils.showToast(R.string.daemon_op_num_inconsistency);
        break;
      default:
        Log.e(TAG, "Wrong command received: " + command);
    }
  }
}
