package com.mirfatif.permissionmanagerx.util;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.os.Build;
import androidx.core.app.NotificationCompat;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.fwk.CrashReportActivityM;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.privtasks.util.LogUtil;
import com.mirfatif.privtasks.util.MyLog;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.concurrent.TimeUnit;

public class LogUtils {

  private static final String TAG = "LogUtils";

  private LogUtils() {}

  public static File getCrashLogFile() {
    String file = "PMXCrash-" + BuildConfig.VERSION_CODE + "-" + Build.VERSION.SDK_INT + ".log";
    return new File(App.getCxt().getExternalFilesDir(null), file);
  }

  public static final int CRASH_FILE_HEADER_LINES = 8;

  public static File createCrashLogFile() {
    File logFile = getCrashLogFile();
    if (!logFile.exists()
        || logFile.length() > 512 * 1024
        || logFile.lastModified() < System.currentTimeMillis() - TimeUnit.DAYS.toMillis(90)) {
      try {
        PrintWriter writer = new PrintWriter(new FileWriter(logFile, false));
        writer.println("=================================");
        writer.println(getDeviceInfo());
        writer.close();
      } catch (IOException ignored) {
      }
    }

    return logFile;
  }

  public static void showCrashNotification(String stackTrace, boolean isDaemon) {
    MySettings.INS.setAskForFeedbackTs(false);

    File logFile = createCrashLogFile();
    try {
      LogUtil.writeCrashLog(logFile, getAppState(), stackTrace, isDaemon);
    } catch (IOException e) {
      MyLog.e(TAG, "showCrashNotification", e);
    }

    if (!MySettings.INS.shouldAskToSendCrashReport() || !ApiUtils.hasNotifPerm()) {
      return;
    }

    final String CHANNEL_ID = "channel_crash_report";
    final String CHANNEL_NAME = ApiUtils.getString(R.string.channel_crash_report);
    final int UNIQUE_ID = ApiUtils.getInt(R.integer.channel_crash_report);

    PendingIntent pi =
        PendingIntent.getActivity(
            App.getCxt(),
            UNIQUE_ID,
            new Intent(App.getCxt(), CrashReportActivityM.class),
            NotifUtils.PI_FLAGS);

    NotificationCompat.Builder builder =
        new NotificationCompat.Builder(App.getCxt(), CHANNEL_ID)
            .setSmallIcon(R.drawable.notification_icon)
            .setContentTitle(ApiUtils.getString(R.string.crash_report))
            .setContentText(ApiUtils.getString(R.string.ask_to_report_crash_small))
            .setStyle(
                new NotificationCompat.BigTextStyle()
                    .bigText(ApiUtils.getString(R.string.ask_to_report_crash)))
            .setContentIntent(pi)
            .addAction(0, ApiUtils.getString(R.string.send_report), pi)
            .setColor(UiUtilsFlavor.getAccentColor())
            .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
            .setPriority(NotificationCompat.PRIORITY_HIGH)
            .setAutoCancel(true);

    NotifUtils.createNotifChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_HIGH);
    NotifUtils.notify(UNIQUE_ID, builder.build());
  }

  public static String getDeviceInfo() {
    return "Version: "
        + BuildConfig.VERSION_NAME
        + "\nSDK: "
        + Build.VERSION.SDK_INT
        + "\nBuild: "
        + Build.TYPE
        + "\nDevice: "
        + Build.DEVICE
        + "\nManufacturer: "
        + Build.MANUFACTURER
        + "\nModel: "
        + Build.MODEL
        + "\nProduct: "
        + Build.PRODUCT;
  }

  public static String getAppState() {
    return "Root: "
        + NativeDaemon.hasRoot(false)
        + "\nADB: "
        + NativeDaemon.hasAdb(false)
        + "\nUID: "
        + DaemonHandler.INS.getUid()
        + "\nContext: "
        + DaemonHandler.INS.getContext();
  }
}
