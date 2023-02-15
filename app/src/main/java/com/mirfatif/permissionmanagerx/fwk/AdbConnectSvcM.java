package com.mirfatif.permissionmanagerx.fwk;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import com.mirfatif.permissionmanagerx.svc.AdbConnectSvc;

public class AdbConnectSvcM extends Service {

  private final AdbConnectSvc mS = new AdbConnectSvc(this);

  public IBinder onBind(Intent intent) {
    return null;
  }

  public int onStartCommand(Intent intent, int flags, int startId) {
    return mS.onStartCommand(intent);
  }
}
