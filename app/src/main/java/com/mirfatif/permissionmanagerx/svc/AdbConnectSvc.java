package com.mirfatif.permissionmanagerx.svc;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getInt;

import android.app.Service;
import android.content.Intent;
import com.mirfatif.err.AdbException;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.fwk.AdbConnectSvcM;
import com.mirfatif.permissionmanagerx.main.AdbConnectDialog;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.AdbConnManager;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonStarter;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.permissionmanagerx.util.AppLifecycle;
import com.mirfatif.permissionmanagerx.util.NotifUtils;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

public class AdbConnectSvc {

  private static final String TAG = "AdbConnectSvc";

  private final AdbConnectSvcM mS;

  public AdbConnectSvc(AdbConnectSvcM svc) {
    mS = svc;
  }

  private static final String EXTRA_HOST = "com.mirfatif.pmx.extra.ADB_HOST";
  private static final String EXTRA_PORT = "com.mirfatif.pmx.extra.ADB_PORT";

  public int onStartCommand(Intent intent) {
    mS.startForeground(
        getInt(R.integer.channel_adb_connection),
        NotifUtils.createSilentFgNotif(
            "channel_adb_connection",
            R.string.channel_adb_connection,
            R.string.adb_conn_notif_title,
            R.string.adb_conn_notif_text));

    if (intent != null) {
      int port = intent.getIntExtra(EXTRA_PORT, AdbConnectDialog.MIN_PORT - 1);
      if (port < AdbConnectDialog.MIN_PORT || port > AdbConnectDialog.MAX_PORT) {
        MyLog.e(TAG, null, "No or bad port received in intent");
      } else if (AppLifecycle.isAppVisible()) {
        MyLog.e(TAG, null, "App is in foreground");
      } else {
        BgRunner.execute(() -> connToAdbAndStartDaemon(intent.getStringExtra(EXTRA_HOST), port));
        return Service.START_NOT_STICKY;
      }
    }

    mS.stopSelf();
    return Service.START_NOT_STICKY;
  }

  private void connToAdbAndStartDaemon(String host, int port) {
    if (DaemonHandler.INS.isDaemonAlive(false, true)) {
      MyLog.w(TAG, null, "Daemon is already running");
      mS.stopSelf();
      return;
    }

    if (NativeDaemon.hasRoot(true) || NativeDaemon.hasAdb(true)) {
      startDaemon();
      return;
    }

    if (host == null) {
      host = MySettings.INS.getAdbHost();
    }

    try (AdbConnManager connMgr = new AdbConnManager()) {
      connMgr.setTimeout(10, TimeUnit.SECONDS);
      if (!connMgr.connect(port)) {
        MyLog.e(TAG, null, "Adb connect to port " + port + " failed");
      } else {
        MySettings.INS.saveAdbPort(port);

        if (NativeDaemon.getAdb(false, false, true, host, port)) {
          startDaemon();
        }
      }
    } catch (AdbException | InterruptedException | IOException e) {
      MyLog.e(TAG, null, e);
    }

    mS.stopSelf();
  }

  private void startDaemon() {
    int res = DaemonStarter.INS.startPrivDaemon(false, false);

    switch (res) {
      case DaemonStarter.DaemonStartStatus.NO_PRIVS:
        MyLog.e(TAG, null, "Failed to get privileges");
        break;
      case DaemonStarter.DaemonStartStatus.FAILED:
        MyLog.e(TAG, null, "Failed to start daemon");
        break;
      default:
        MyLog.i(TAG, null, "Daemon started");
    }
  }
}
