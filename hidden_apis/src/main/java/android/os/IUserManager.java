package android.os;

import android.content.pm.UserInfo;
import java.util.List;

public interface IUserManager {

  // Was added in Android-10.0.0_r30
  List<UserInfo> getUsers(boolean excludePartial, boolean excludeDying, boolean excludePreCreated)
      throws RemoteException;

  List<UserInfo> getUsers(boolean excludeDying) throws RemoteException;

  abstract class Stub {

    @SuppressWarnings("UnusedDeclaration")
    public static IUserManager asInterface(IBinder obj) {
      return null;
    }
  }
}
