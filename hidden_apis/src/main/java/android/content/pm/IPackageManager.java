package android.content.pm;

import android.os.IBinder;

public interface IPackageManager {

  ParceledListSlice<?> getAllPermissionGroups(int i);

  ParceledListSlice<?> queryPermissionsByGroup(String s, int i);

  int getPermissionFlags(String s1, String s2, int i);

  void grantRuntimePermission(String s1, String s2, int i);

  void revokeRuntimePermission(String s1, String s2, int i);

  void setApplicationEnabledSetting(String s1, int i1, int i2, int i3, String s2);

  String[] getPackagesForUid(int i);

  abstract class Stub {

    public static IPackageManager asInterface(IBinder o) {
      return null;
    }
  }
}
