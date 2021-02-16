package android.permission;

import android.content.pm.ParceledListSlice;
import android.os.IBinder;
import android.os.RemoteException;

public interface IPermissionManager {

  ParceledListSlice<?> getAllPermissionGroups(int flags) throws RemoteException;

  ParceledListSlice<?> queryPermissionsByGroup(String permGroup, int flags) throws RemoteException;

  int getPermissionFlags(String permName, String pkgName, int userId) throws RemoteException;

  void revokeRuntimePermission(String pkgName, String permName, int userId, String reason)
      throws RemoteException;

  void updatePermissionFlags(
      String permName,
      String packageName,
      int flagMask,
      int flagValues,
      boolean checkAdjustPolicyFlagPermission,
      int userId)
      throws RemoteException;

  abstract class Stub {

    @SuppressWarnings("UnusedDeclaration")
    public static IPermissionManager asInterface(IBinder obj) {
      return null;
    }
  }
}
