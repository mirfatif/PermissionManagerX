package android.permission;

import android.content.pm.ParceledListSlice;
import android.os.IBinder;

public interface IPermissionManager {

  ParceledListSlice<?> getAllPermissionGroups(int i);

  ParceledListSlice<?> queryPermissionsByGroup(String s, int i);

  int getPermissionFlags(String s1, String s2, int i);

  void revokeRuntimePermission(String s1, String s2, int i, String s3);

  abstract class Stub {

    public static IPermissionManager asInterface(IBinder o) {
      return null;
    }
  }
}
