package com.mirfatif.permissionmanagerx.util;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.Signature;
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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.security.MessageDigest;
import java.security.cert.CertificateFactory;
import java.util.concurrent.TimeUnit;

public class LogUtils {

  private static final String TAG = "LogUtils";

  private LogUtils() {}

  public static File getCrashLogFile() {
    String file = "PMXCrash-" + BuildConfig.VERSION_CODE + "-" + Build.VERSION.SDK_INT + ".log";
    var filesDir = App.getCxt().getExternalFilesDir(null);
    if (filesDir == null) {
      filesDir = App.getCxt().getExternalFilesDir(null);
    }
    return new File(filesDir, file);
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
    final String CHANNEL_NAME = getString(R.string.channel_crash_report);
    final int UNIQUE_ID = ApiUtils.getInt(R.integer.channel_crash_report);

    var isOfficialRelease = isOfficialRelease();
    var msg =
        getString(
            isOfficialRelease ? R.string.ask_to_report_crash_small : R.string.inform_report_crash);
    var bigMsg = isOfficialRelease ? getString(R.string.ask_to_report_crash) : msg;

    NotificationCompat.Builder builder =
        new NotificationCompat.Builder(App.getCxt(), CHANNEL_ID)
            .setSmallIcon(R.drawable.notification_icon)
            .setContentTitle(getString(R.string.crash_report))
            .setContentText(msg)
            .setStyle(new NotificationCompat.BigTextStyle().bigText(bigMsg))
            .setColor(UiUtils.getAccentColor())
            .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
            .setPriority(NotificationCompat.PRIORITY_HIGH)
            .setAutoCancel(true);

    if (isOfficialRelease) {
      PendingIntent pi =
          PendingIntent.getActivity(
              App.getCxt(),
              UNIQUE_ID,
              new Intent(App.getCxt(), CrashReportActivityM.class),
              NotifUtils.PI_FLAGS);

      builder.setContentIntent(pi).addAction(0, getString(R.string.send_report), pi);
    }

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

  private static final String CERT_FINGERPRINT =
      "89:B9:ED:EE:D8:E4:52:32:25:9D:12:D7:97:7C:FB:C5:E6:67:15:39:A1:FF:54:BB:FB:4F:88:CD:3E:90:0C:55";

  // apksigner verify --print-certs app-release.apk
  public static boolean isOfficialRelease() {
    int flags =
        (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P)
            ? PackageManager.GET_SIGNING_CERTIFICATES
            : PackageManager.GET_SIGNATURES;
    PackageInfo info;
    try {
      info = App.getPm().getPackageInfo(App.getCxt().getPackageName(), flags);
    } catch (PackageManager.NameNotFoundException e) {
      MyLog.e(TAG, "isOfficialRelease", e);
      return false;
    }

    Signature[] signatures;
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      if (info.signingInfo != null) {
        signatures = info.signingInfo.getApkContentsSigners();
      } else {
        MyLog.e(TAG, "isOfficialRelease", "App signingInfo not found");
        return false;
      }
    } else {
      signatures = info.signatures;
    }

    if (signatures == null || signatures.length == 0) {
      MyLog.e(TAG, "isOfficialRelease", "APK signatures not found");
      return false;
    }

    try {
      CertificateFactory cf = CertificateFactory.getInstance("X.509");
      MessageDigest md = MessageDigest.getInstance("SHA-256");
      for (Signature sig : signatures) {
        var cert = cf.generateCertificate(new ByteArrayInputStream(sig.toByteArray()));
        byte[] digest = md.digest(cert.getEncoded());
        StringBuilder sb = new StringBuilder();
        for (byte b : digest) {
          sb.append(String.format("%02X", b));
        }
        if (sb.toString().equalsIgnoreCase(CERT_FINGERPRINT.replace(":", ""))) {
          return true;
        }
      }
    } catch (Exception e) {
      MyLog.e(TAG, "isOfficialRelease", e);
      return false;
    }
    return false;
  }
}
