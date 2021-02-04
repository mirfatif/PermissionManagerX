package com.mirfatif.permissionmanagerx.svc;

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
import android.widget.Toast;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationCompat.Builder;
import androidx.core.app.NotificationManagerCompat;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.main.MainActivity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.Adb;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.permissionmanagerx.util.Utils;
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

  private final MySettings mMySettings = MySettings.getInstance();
  private final PrivDaemonHandler mDaemonHandler = PrivDaemonHandler.getInstance();

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
    return super.onStartCommand(intent, flags, startId);
  }

  private CountDownTimer mTimer;
  private NotificationManagerCompat mNotificationManager;
  private Builder mNotificationBuilder;

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
            PendingIntent.FLAG_UPDATE_CURRENT);

    mNotificationBuilder = new Builder(App.getContext(), CHANNEL_ID);
    mNotificationBuilder
        .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
        .setPriority(NotificationCompat.PRIORITY_HIGH)
        .setOnlyAlertOnce(true)
        .setSmallIcon(R.drawable.notification_icon)
        .setContentTitle(Utils.getString(R.string.logging))
        .setColor(Utils.getAccentColor())
        .setStyle(
            new NotificationCompat.BigTextStyle().bigText(getString(R.string.logging_warning)))
        .addAction(0, getString(R.string.stop_logging), stopIntent);

    startForeground(getUniqueId(), mNotificationBuilder.build());
  }

  private int getUniqueId() {
    return Utils.getInteger(R.integer.channel_logcat_collection);
  }

  private static final int TIMEOUT_SEC = 5 * 60;

  private void setNotificationProgress(int now) {
    if (mNotificationManager == null || mNotificationBuilder == null) {
      return;
    }
    int min = now / 60;
    String text = String.format(Locale.getDefault(), "%02d:%02d", min, now - min * 60);
    mNotificationBuilder.setProgress(TIMEOUT_SEC, now, false);
    mNotificationBuilder.setContentText(text);
    mNotificationManager.notify(getUniqueId(), mNotificationBuilder.build());
  }

  private void stopSvc() {
    stopSelf();
    if (mTimer != null) {
      mTimer.cancel();
      mTimer = null;
    }
  }

  private void stopSvcAndShowFailed() {
    stopSvc();
    Utils.runInFg(
        () -> Toast.makeText(App.getContext(), R.string.logging_failed, Toast.LENGTH_LONG).show());
  }

  private void stopLoggingAndSvc() {
    stopSvc();
    stopLogging();
  }

  private void stopLogging() {
    synchronized (LOG_WRITER_LOCK) {
      if (!mMySettings.isDebug()) {
        return;
      }
      mMySettings.setDebugLog(false);
      if (mLogcatWriter != null) {
        mLogcatWriter.close();
      }
      mLogcatWriter = null;
      if (mMySettings.isPrivDaemonAlive()) {
        // Stop daemon logging
        mDaemonHandler.sendRequest(Commands.STOP_LOGGING);
      }
      // Stop app logging
      Log.i(TAG, "stopLogging: please " + Commands.STOP_LOGGING);
    }
  }

  public static void sendStopLogIntent() {
    // Intent without start action would stop the service and logging.
    App.getContext().startService(new Intent(App.getContext(), LogcatService.class));
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
      mLogcatWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outStream)));
    } catch (IOException e) {
      e.printStackTrace();
      stopSvcAndShowFailed();
      return;
    }

    writeToLogFile(Utils.getDeviceInfo());

    if (mMySettings.isPrivDaemonAlive()) {
      // We'll start it in MainActivity to log all of the start messages.
      mDaemonHandler.sendRequest(Commands.SHUTDOWN);
    }

    Utils.runCommand(TAG + ": doLogging", null, null, "logcat", "-c");
    Log.d(TAG, "doLogging: starting");

    if (doLoggingFails("sh", "exec logcat --pid " + android.os.Process.myPid())) {
      stopSvcAndShowFailed();
      return;
    }

    Utils.runInFg(this::startTimer);
    mMySettings.setDebugLog(true);

    Intent intent = new Intent(App.getContext(), MainActivity.class);
    intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
    Utils.runInFg(() -> startActivity(intent));
  }

  public static boolean doLoggingFails(String... cmd) {
    if (cmd.length != 2) {
      Log.e(TAG, "doLogging: command length must be 2");
      return true;
    }

    Process process = Utils.runCommand(TAG + ": doLogging", true, cmd[0]);
    if (process == null) {
      return true;
    }

    Utils.runInBg(() -> readLogcatStream(process, null));

    Log.i(TAG, "doLogging: sending command to shell: " + cmd[1]);
    new PrintWriter(process.getOutputStream(), true).println(cmd[1]);

    return false;
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
      Utils.cleanProcess(reader, process, adb, TAG + ": readLogcatStream");
    }
  }

  private static PrintWriter mLogcatWriter;
  private static final Object LOG_WRITER_LOCK = new Object();

  private static void writeToLogFile(String line) {
    synchronized (LOG_WRITER_LOCK) {
      if (mLogcatWriter == null) {
        return;
      }
      if (!MySettings.getInstance().isDebug()) {
        return;
      }
      mLogcatWriter.println(line);
    }
  }

  public static void appCrashed() {
    synchronized (LOG_WRITER_LOCK) {
      if (mLogcatWriter != null) {
        SystemClock.sleep(1000);
        mLogcatWriter.flush();
      }
    }
  }
}
