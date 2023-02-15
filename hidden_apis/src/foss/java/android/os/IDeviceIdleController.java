package android.os;

public interface IDeviceIdleController {

  void addPowerSaveWhitelistApp(String s);

  abstract class Stub {

    public static IDeviceIdleController asInterface(IBinder o) {
      return null;
    }
  }
}
