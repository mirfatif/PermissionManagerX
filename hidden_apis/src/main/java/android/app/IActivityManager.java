package android.app;

import android.content.ComponentName;
import android.content.Intent;
import android.os.Bundle;
import android.os.IBinder;

public interface IActivityManager {

  int startActivityAsUser(
      IApplicationThread o1,
      String o2,
      Intent o3,
      String o4,
      IBinder o5,
      String o6,
      int i1,
      int i2,
      ProfilerInfo o7,
      Bundle o8,
      int i3);

  ComponentName startService(
      IApplicationThread o1, Intent o2, String o3, boolean b, String o4, String o5, int i);

  ComponentName startService(
      IApplicationThread o1, Intent o2, String o3, boolean b, String o4, int i);

  ComponentName startService(IApplicationThread o1, Intent o2, String o3, String o4, int i);

  int checkPermission(String s, int i1, int i2);

  abstract class Stub {

    public static IActivityManager asInterface(IBinder o) {
      return null;
    }
  }
}

class IApplicationThread {}

class ProfilerInfo {}
