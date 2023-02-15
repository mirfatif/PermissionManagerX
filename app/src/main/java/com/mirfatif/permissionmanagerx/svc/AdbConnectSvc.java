package com.mirfatif.permissionmanagerx.svc;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getInt;
import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.app.NotificationManager;
import android.app.Service;
import android.content.Intent;
import androidx.core.app.NotificationCompat;
import com.mirfatif.err.AdbException;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.fwk.AdbConnectSvcM;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.AdbConnManager;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonStarter;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.permissionmanagerx.util.AppLifecycle;
import com.mirfatif.permissionmanagerx.util.NotifUtils;
import com.mirfatif.permissionmanagerx.util.UiUtilsFlavor;
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
    final String CHANNEL_ID = "channel_adb_connection";
    final String CHANNEL_NAME = getString(R.string.channel_adb_connection);

    NotifUtils.createNotifChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_NONE);

    NotificationCompat.Builder builder = new NotificationCompat.Builder(App.getCxt(), CHANNEL_ID);
    builder
        .setPriority(NotificationCompat.PRIORITY_MIN)
        .setSilent(true)
        .setContentTitle(getString(R.string.adb_conn_notif_title))
        .setContentText(getString(R.string.adb_conn_notif_text))
        .setSmallIcon(R.drawable.notification_icon)
        .setColor(UiUtilsFlavor.getAccentColor());

    mS.startForeground(getInt(R.integer.channel_adb_connection), builder.build());

    if (intent != null) {
      int port = intent.getIntExtra(EXTRA_PORT, 0);
      if (port == 0) {
        MyLog.e(TAG, null, "No port received in intent");
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
