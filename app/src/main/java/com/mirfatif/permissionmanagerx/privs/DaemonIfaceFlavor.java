package com.mirfatif.permissionmanagerx.privs;

import android.os.IBinder;

public enum DaemonIfaceFlavor {
  INS;

  public void onDaemonStarted(IBinder privTasksFlavor) {}

  public void onDaemonStopped() {}
}
