package com.mirfatif.permissionmanagerx.svc;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;
import static com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler.DAEMON_HANDLER;
import static com.mirfatif.permissionmanagerx.util.Utils.PI_FLAGS;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.net.Uri;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.CountDownTimer;
import android.os.IBinder;
import android.os.SystemClock;
import android.util.Log;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationCompat.Builder;
import androidx.core.app.NotificationManagerCompat;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.main.fwk.MainActivity;
import com.mirfatif.permissionmanagerx.privs.Adb;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.permissionmanagerx.util.UtilsFlavor;
import com.mirfatif.privtasks.Commands;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Locale;

public class LogcatService extends Service {

  private static final String TAG = "LogcatService";

  public static final String ACTION_START_LOG = BuildConfig.APPLICATION_ID + ".START_LOGCAT";

  @Override
  public IBinder onBind(Intent intent) {
    return null;
  }

  private final Object ON_START_CMD_LOCK = new Object();

  @Override
  public int onStartCommand(Intent intent, int flags, int startId) {
    Utils.runInBg(
        () -> {
          synchronized (ON_START_CMD_LOCK) {
            String action = intent.getAction();
            Uri logFile = intent.getData();
            if (action != null && action.equals(ACTION_START_LOG) && logFile != null) {
              doLogging(logFile);
              startSvc();
            } else {
              stopLoggingAndSvc();
            }
          }
        });

    // Do not restart the service if the process is killed
    return Service.START_NOT_STICKY;
  }

  @Override
  public void onDestroy() {
    stopLoggingAndSvc();
    super.onDestroy();
  }

  private CountDownTimer mTimer;
  private NotificationManagerCompat mNotificationManager;
  private Builder mNotificationBuilder;
  private final Object NOTIF_BUILDER_LOCK = new Object();

  private void startSvc() {
    final String CHANNEL_ID = "channel_logcat_collection";
    final String CHANNEL_NAME = Utils.getString(R.string.channel_logcat_collection);

    mNotificationManager = NotificationManagerCompat.from(App.getContext());
    NotificationChannel channel = mNotificationManager.getNotificationChannel(CHANNEL_ID);
    if (channel == null && VERSION.SDK_INT >= VERSION_CODES.O) {
      channel =
          new NotificationChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_HIGH);
      mNotificationManager.createNotificationChannel(channel);
    }

    PendingIntent stopIntent =
        PendingIntent.getService(
            App.getContext(),
            getUniqueId(),
            new Intent(App.getContext(), LogcatService.class),
            PI_FLAGS);

    synchronized (NOTIF_BUILDER_LOCK) {
      mNotificationBuilder = new Builder(App.getContext(), CHANNEL_ID);
      mNotificationBuilder
          .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
          .setPriority(NotificationCompat.PRIORITY_HIGH)
          .setOnlyAlertOnce(true)
          .setSmallIcon(R.drawable.notification_icon)
          .setContentTitle(Utils.getString(R.string.logging))
          .setColor(UtilsFlavor.getAccentColor())
          .setStyle(
              new NotificationCompat.BigTextStyle().bigText(getString(R.string.logging_warning)))
          .addAction(0, getString(R.string.stop_logging), stopIntent);

      startForeground(getUniqueId(), mNotificationBuilder.build());
    }
  }

  private int getUniqueId() {
    return Utils.getInteger(R.integer.channel_logcat_collection);
  }

  private static final int TIMEOUT_SEC = 5 * 60;

  private void setNotificationProgress(int now) {
    synchronized (NOTIF_BUILDER_LOCK) {
      if (mNotificationManager == null || mNotificationBuilder == null) {
        return;
      }
      int min = now / 60;
      String text = String.format(Locale.getDefault(), "%02d:%02d", min, now - min * 60);
      mNotificationBuilder.setProgress(TIMEOUT_SEC, now, false);
      mNotificationBuilder.setContentText(text);
      mNotificationManager.notify(getUniqueId(), mNotificationBuilder.build());
    }
  }

  private void stopSvc() {
    synchronized (NOTIF_BUILDER_LOCK) {
      mNotificationBuilder = null;
    }
    stopSelf();
    if (mTimer != null) {
      mTimer.cancel();
      mTimer = null;
    }
  }

  private void stopSvcAndShowFailed() {
    stopSvc();
    Utils.showToast(R.string.logging_failed);
  }

  private void stopLoggingAndSvc() {
    stopSvc();
    stopLogging(true);
  }

  private void stopLogging(boolean sendCmd) {
    synchronized (LOG_WRITER_LOCK) {
      if (!SETTINGS.isDebug()) {
        return;
      }
      SETTINGS.setDebugLog(false);
      if (sLogcatWriter != null) {
        sLogcatWriter.close();
      }
      sLogcatWriter = null;

      if (!sendCmd) {
        return;
      }

      if (SETTINGS.isPrivDaemonAlive()) {
        // Stop daemon logging
        DAEMON_HANDLER.sendRequest(Commands.STOP_LOGGING);
      }
      // Stop app logging
      Log.i(TAG, "stopLogging: please " + Commands.STOP_LOGGING);
    }
  }

  public static void sendStopLogIntent() {
    App.getContext().stopService(new Intent(App.getContext(), LogcatService.class));
  }

  private void startTimer() {
    mTimer =
        new CountDownTimer(TIMEOUT_SEC * 1000, 1000) {
          @Override
          public void onTick(long millisUntilFinished) {
            Utils.runInBg(() -> setNotificationProgress((int) (millisUntilFinished / 1000)));
          }

          @Override
          public void onFinish() {
            Utils.runInBg(() -> stopLoggingAndSvc());
          }
        };
    mTimer.start();
  }

  private void doLogging(Uri logFile) {
    try {
      OutputStream outStream =
          getApplication().getContentResolver().openOutputStream(logFile, "rw");
      sLogcatWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outStream)));
    } catch (IOException e) {
      e.printStackTrace();
      stopSvcAndShowFailed();
      return;
    }

    SETTINGS.setDebugLog(true);
    writeToLogFile(Utils.getDeviceInfo());
    writeToLogFile("=====================================");

    if (SETTINGS.isPrivDaemonAlive()) {
      // We'll start it in MainActivity to log all of the start messages.
      DAEMON_HANDLER.sendRequest(Commands.SHUTDOWN);
    }

    Utils.runCommand(TAG + ": doLogging", "logcat", "-c");
    Log.d(TAG, "doLogging: Starting");

    if (!doLogging("sh", "exec logcat --pid " + android.os.Process.myPid())) {
      stopSvcAndShowFailed();
      stopLogging(false);
      return;
    }

    Utils.runInFg(this::startTimer);

    Intent intent = new Intent(App.getContext(), MainActivity.class);
    intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
    Utils.runInFg(() -> startActivity(intent));
  }

  @SuppressWarnings("BooleanMethodIsAlwaysInverted")
  public static boolean doLogging(String cmd1, String cmd2) {
    Process process = Utils.runCommand(TAG + ": doLogging", true, cmd1);
    if (process == null) {
      return false;
    }

    Utils.runInBg(() -> readLogcatStream(process, null));

    Log.i(TAG, "doLogging: sending command to shell: " + cmd2);
    new PrintWriter(process.getOutputStream(), true).println(cmd2);

    return true;
  }

  public static void readLogcatStream(Process process, Adb adb) {
    BufferedReader reader;
    if (process != null) {
      reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
    } else if (adb != null) {
      reader = new BufferedReader(adb.getReader());
    } else {
      return;
    }

    try {
      String line;
      while ((line = reader.readLine()) != null && !line.contains(Commands.STOP_LOGGING)) {
        writeToLogFile(line);
      }
    } catch (IOException e) {
      Log.e(TAG, "readLogcatStream: " + e.toString());
    } finally {
      // If process exited itself
      sendStopLogIntent();
      Utils.cleanStreams(process, adb, TAG + ": readLogcatStream");
    }
  }

  private static PrintWriter sLogcatWriter;
  private static final Object LOG_WRITER_LOCK = new Object();
  private static int sLinesWritten;

  private static void writeToLogFile(String line) {
    synchronized (LOG_WRITER_LOCK) {
      if (sLogcatWriter == null) {
        return;
      }
      if (!SETTINGS.isDebug()) {
        return;
      }
      sLogcatWriter.println(line);

      // Let's be flash friendly
      sLinesWritten++;
      if (sLinesWritten >= 100) {
        sLogcatWriter.flush();
        sLinesWritten = 0;
      }
    }
  }

  public static void appCrashed() {
    synchronized (LOG_WRITER_LOCK) {
      if (sLogcatWriter != null) {
        SystemClock.sleep(1000);
        sLogcatWriter.flush();
      }
    }
  }
}
