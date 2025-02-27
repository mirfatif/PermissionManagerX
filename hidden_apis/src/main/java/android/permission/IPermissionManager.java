package android.permission;

import android.content.pm.ParceledListSlice;
import android.os.IBinder;

public interface IPermissionManager {

  ParceledListSlice<?> getAllPermissionGroups(int i);

  ParceledListSlice<?> queryPermissionsByGroup(String s, int i);

  int getPermissionFlags(String s1, String s2, int i);

  int getPermissionFlags(String s1, String s2, int i1, int i2);

  int getPermissionFlags(String s1, String s2, String s3, int i2);

  void grantRuntimePermission(String s1, String s2, int i);

  void grantRuntimePermission(String s1, String s2, int i1, int i2);

  void grantRuntimePermission(String s1, String s2, String s3, int i2);

  void revokeRuntimePermission(String s1, String s2, int i, String s3);

  void revokeRuntimePermission(String s1, String s2, int i1, int i2, String s3);

  void revokeRuntimePermission(String s1, String s2, String s3, int i2, String s4);

  abstract class Stub {

    public static IPermissionManager asInterface(IBinder o) {
      return null;
    }
  }
}
