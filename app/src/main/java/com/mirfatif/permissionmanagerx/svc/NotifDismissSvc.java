package com.mirfatif.permissionmanagerx.svc;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import androidx.core.app.NotificationManagerCompat;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.app.App;

public class NotifDismissSvc extends Service {

  public static final String EXTRA_INTENT_TYPE = BuildConfig.APPLICATION_ID + ".EXTRA_INTENT_TYPE";
  public static final String EXTRA_NOTIF_ID = BuildConfig.APPLICATION_ID + ".EXTRA_NOTIF_ID";
  public static final int INTENT_TYPE_ACTIVITY = 1;
  public static final int INTENT_TYPE_SERVICE = 2;
  private static final int NONE = -1;

  @Override
  public IBinder onBind(Intent intent) {
    return null;
  }

  @Override
  public int onStartCommand(Intent intent, int flags, int startId) {
    int type = intent.getIntExtra(EXTRA_INTENT_TYPE, NONE);
    int id = intent.getIntExtra(EXTRA_NOTIF_ID, NONE);
    if (type != NONE && id != NONE) {
      NotificationManagerCompat.from(App.getContext()).cancel(id);
      intent.setComponent(null);
      intent.removeExtra(EXTRA_INTENT_TYPE);
      intent.removeExtra(EXTRA_NOTIF_ID);
      // FLAG_ACTIVITY_NEW_TASK is required to start Activity from outside an Activity
      intent.setFlags(intent.getFlags() | Intent.FLAG_ACTIVITY_NEW_TASK);
      if (type == INTENT_TYPE_ACTIVITY) {
        startActivity(intent);
      } else if (type == INTENT_TYPE_SERVICE) {
        startService(intent);
      }
    }
    stopSelf(startId); // Stop if no pending requests
    return Service.START_NOT_STICKY;
  }
}
