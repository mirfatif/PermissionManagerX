package com.mirfatif.permissionmanagerx.fwk;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.privtasks.util.bg.BgRunner;

public class DaemonRcvSvcM extends Service {

  public IBinder onBind(Intent intent) {
    return null;
  }

  public int onStartCommand(Intent intent, int flags, int startId) {
    BgRunner.execute(() -> DaemonHandler.INS.onBinderReceived(intent));
    stopSelf();
    return Service.START_NOT_STICKY;
  }
}
