package com.mirfatif.permissionmanagerx.fwk;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import com.mirfatif.permissionmanagerx.svc.LogcatSvc;

public class LogcatSvcM extends Service {

  private final LogcatSvc mS = new LogcatSvc(this);

  public IBinder onBind(Intent intent) {
    return null;
  }

  public int onStartCommand(Intent intent, int flags, int startId) {
    return mS.onStartCommand(intent);
  }

  public void onDestroy() {
    mS.onDestroy();
    super.onDestroy();
  }
}
