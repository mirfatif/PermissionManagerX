package com.mirfatif.permissionmanagerx.util;

import static android.app.PendingIntent.FLAG_IMMUTABLE;
import static android.app.PendingIntent.FLAG_UPDATE_CURRENT;
import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;
import static com.mirfatif.permissionmanagerx.util.ApiUtils.hasNotifPerm;

import android.Manifest;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.os.Build;
import androidx.activity.result.ActivityResultLauncher;
import androidx.appcompat.app.AlertDialog;
import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.fragment.app.FragmentActivity;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.prefs.MySettings;

public class NotifUtils {

  public static final int PI_FLAGS = FLAG_UPDATE_CURRENT | FLAG_IMMUTABLE;
  private static final String NOTIFICATION_PERM_DIALOG_TAG = "NOTIFICATION_PERM_DIALOG_TAG";

  private NotifUtils() {}

  public static void askForNotifPerm(
      FragmentActivity activity, ActivityResultLauncher<String> reqPermLauncher) {
    if (!ApiUtils.hasNotifPerm()) {
      String perm = Manifest.permission.POST_NOTIFICATIONS;

      if (!ActivityCompat.shouldShowRequestPermissionRationale(activity, perm)) {
        UiUtils.showToast(R.string.notif_perm_missing_toast);
        reqPermLauncher.launch(perm);
      } else {
        AlertDialog.Builder builder =
            new AlertDialog.Builder(activity)
                .setTitle(R.string.notif_perm_dialog_title)
                .setMessage(StringUtils.htmlToString(R.string.notif_perm_dialog_text))
                .setPositiveButton(R.string.ok_button, (d, w) -> reqPermLauncher.launch(perm))
                .setNegativeButton(R.string.cancel_button, null);

        AlertDialogFragment.show(activity, builder.create(), NOTIFICATION_PERM_DIALOG_TAG);
      }
    }

    MySettings.INS.setAskForNotifPermTs();
  }

  public static void createNotifChannel(String id, String name, int importance) {
    NotificationManagerCompat nm = NotificationManagerCompat.from(App.getCxt());
    NotificationChannel channel = nm.getNotificationChannel(id);
    if (channel == null && Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      channel = new NotificationChannel(id, name, importance);
      nm.createNotificationChannel(channel);
    }
  }

  public static Notification createSilentFgNotif(
      String channelId, int channelName, int title, int text) {
    NotifUtils.createNotifChannel(
        channelId, getString(channelName), NotificationManager.IMPORTANCE_NONE);

    NotificationCompat.Builder builder = new NotificationCompat.Builder(App.getCxt(), channelId);
    builder
        .setPriority(NotificationCompat.PRIORITY_MIN)
        .setSilent(true)
        .setContentTitle(getString(title))
        .setContentText(getString(text))
        .setSmallIcon(R.drawable.notification_icon)
        .setColor(UiUtils.getAccentColor());

    return builder.build();
  }

  public static void notify(int id, Notification notif) {
    if (hasNotifPerm()) {
      NotificationManagerCompat.from(App.getCxt()).notify(id, notif);
    }
  }
}
