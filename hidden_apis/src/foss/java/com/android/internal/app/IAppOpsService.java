package com.android.internal.app;

import android.app.AppOpsManager.PackageOps;
import android.os.IBinder;
import java.util.List;

public interface IAppOpsService {

  void setMode(int i1, int i2, String s, int i3);

  void setUidMode(int i1, int i2, int i3);

  void resetAllModes(int i, String s);

  List<PackageOps> getUidOps(int i, int[] o);

  List<PackageOps> getOpsForPackage(int i, String s, int[] o);

  abstract class Stub {

    public static IAppOpsService asInterface(IBinder o) {
      return null;
    }
  }
}
