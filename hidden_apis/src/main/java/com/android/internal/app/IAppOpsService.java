package com.android.internal.app;

import android.app.AppOpsManager.PackageOps;
import android.os.IBinder;
import android.os.RemoteException;
import java.util.List;

public interface IAppOpsService {

  void setMode(int op, int uid, String pkgName, int mode) throws RemoteException;

  void setUidMode(int op, int uid, int mode) throws RemoteException;

  void resetAllModes(int userId, String pkgName) throws RemoteException;

  List<PackageOps> getUidOps(int uid, int[] ops) throws RemoteException;

  List<PackageOps> getOpsForPackage(int uid, String packageName, int[] ops) throws RemoteException;

  abstract class Stub {

    @SuppressWarnings("UnusedDeclaration")
    public static IAppOpsService asInterface(IBinder obj) {
      return null;
    }
  }
}
