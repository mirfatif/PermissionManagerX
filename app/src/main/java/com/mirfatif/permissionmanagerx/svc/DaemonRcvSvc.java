package com.mirfatif.permissionmanagerx.svc;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getInt;

import android.app.Service;
import android.content.Intent;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.fwk.DaemonRcvSvcM;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.util.NotifUtils;
import com.mirfatif.privtasks.util.bg.BgRunner;

public class DaemonRcvSvc {

  private final DaemonRcvSvcM mS;

  public DaemonRcvSvc(DaemonRcvSvcM svc) {
    mS = svc;
  }

  public int onStartCommand(Intent intent) {
    BgRunner.execute(() -> DaemonHandler.INS.onBinderReceived(intent));

    mS.startForeground(
        getInt(R.integer.channel_daemon_connection),
        NotifUtils.createSilentFgNotif(
            "channel_daemon_connection",
            R.string.channel_daemon_connection,
            R.string.daemon_connection_title,
            R.string.daemon_connection_text));

    mS.stopSelf();

    return Service.START_NOT_STICKY;
  }
}
