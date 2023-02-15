package com.mirfatif.permissionmanagerx.svc;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.net.Uri;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.CountDownTimer;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationCompat.Builder;
import androidx.core.app.NotificationManagerCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.fwk.LogcatSvcM;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.LogUtils;
import com.mirfatif.permissionmanagerx.util.NotifUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.UiUtilsFlavor;
import com.mirfatif.permissionmanagerx.util.bg.UiRunner;
import com.mirfatif.privtasks.bind.ILogCallback;
import com.mirfatif.privtasks.util.LogUtil;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.bg.BgRunner;
import com.mirfatif.privtasks.util.bg.SingleParamTask;
import com.mirfatif.privtasks.util.bg.SingleTaskExecutor;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicBoolean;

public class LogcatSvc {

  private static final String TAG = "LogcatSvc";

  private final LogcatSvcM mS;

  public LogcatSvc(LogcatSvcM svc) {
    mS = svc;
  }

  private static final String ACTION_STOP = LogcatSvc.class.getName() + ".A";
  private static final String ACTION_SEND_CALLBACK = LogcatSvc.class.getName() + ".B";

  public int onStartCommand(Intent intent) {
    if (ACTION_STOP.equals(intent.getAction())) {
      mS.stopSelf();
      return Service.START_NOT_STICKY;
    }

    createSvcNotification();
    mS.startForeground(UNIQUE_ID, mNotifBuilder.build());

    if (ACTION_SEND_CALLBACK.equals(intent.getAction())) {
      sendDmnCallback();
    } else {
      Uri logFile = intent.getData();
      if (logFile != null) {
        BgRunner.execute(() -> startLogging(logFile));
      }
    }

    return Service.START_NOT_STICKY;
  }

  public void onDestroy() {
    synchronized (LIVE) {
      LIVE.set(false);
    }

    mLogExecutor.cancel(true);

    if (DaemonHandler.INS.isDaemonAlive()) {
      BgRunner.execute(() -> DaemonIface.INS.setDebug(null));
    }

    mCallback.mSvc = null;

    mNotifMgr.cancel(UNIQUE_ID);

    mTimer.cancel();

    mLogWriter.close();

    MySettings.INS.setDebugLog(false);
  }

  private final NotificationManagerCompat mNotifMgr = NotificationManagerCompat.from(App.getCxt());
  private Builder mNotifBuilder;

  private static final int UNIQUE_ID = ApiUtils.getInt(R.integer.channel_logcat_collection);

  private void createSvcNotification() {
    if (mNotifBuilder != null) {
      return;
    }

    final String CHANNEL_ID = "channel_logcat_collection";
    final String CHANNEL_NAME = getString(R.string.channel_logcat_collection);

    NotifUtils.createNotifChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_HIGH);

    Intent i = new Intent(App.getCxt(), LogcatSvcM.class).setAction(ACTION_STOP);
    PendingIntent pi = PendingIntent.getService(App.getCxt(), UNIQUE_ID, i, NotifUtils.PI_FLAGS);

    mNotifBuilder = new Builder(App.getCxt(), CHANNEL_ID);
    mNotifBuilder
        .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
        .setPriority(NotificationCompat.PRIORITY_HIGH)
        .setOnlyAlertOnce(true)
        .setSmallIcon(R.drawable.notification_icon)
        .setContentTitle(getString(R.string.logging_title))
        .setColor(UiUtilsFlavor.getAccentColor())
        .setStyle(
            new NotificationCompat.BigTextStyle().bigText(getString(R.string.logging_warning_msg)))
        .addAction(0, getString(R.string.stop_button), pi)
        .setOngoing(true);
  }

  private final AtomicBoolean LIVE = new AtomicBoolean(false);

  private void startLogging(Uri logFile) {
    synchronized (LIVE) {
      if (LIVE.get()) {
        return;
      }
      LIVE.set(true);

      try {
        OutputStream os = App.getCxt().getContentResolver().openOutputStream(logFile, "rw");
        mLogWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(os)));
      } catch (FileNotFoundException e) {
        MyLog.e(TAG, "startLogging", e);
        onFailed();
        return;
      }

      MySettings.INS.setDebugLog(true);

      writeToLogFile(LogUtils.getDeviceInfo());
      writeToLogFile(LogUtils.getAppState());
      writeToLogFile("=====================================");

      mLogExecutor = LogUtil.readLogcat(this::writeToLogFile);
      if (mLogExecutor == null) {
        onFailed();
        return;
      }

      UiRunner.post(mTimer::start);
    }

    sendDmnCallback();
  }

  private final LogCallback mCallback = new LogCallback(this);

  private void sendDmnCallback() {
    if (DaemonHandler.INS.isDaemonAlive()) {
      BgRunner.execute(() -> DaemonIface.INS.setDebug(mCallback));
    }
  }

  private void onFailed() {
    UiUtils.showToast(R.string.logging_failed_toast);
    mS.stopSelf();
  }

  private final CountDownTimer mTimer = new Timer();

  private class Timer extends CountDownTimer {

    private Timer() {
      super(TIMEOUT_SEC * 1000, 1000);
    }

    private final SingleParamTask<Integer> mNotifUpdater =
        new SingleParamTask<>(this::setNotificationProgress, TAG + "-NotifTickUpdater");

    public void onTick(long millisUntilFinished) {
      mNotifUpdater.submitIfIdle((int) (millisUntilFinished / 1000));
    }

    public void onFinish() {
      mS.stopSelf();
    }

    private static final int TIMEOUT_SEC = 5 * 60;

    private void setNotificationProgress(int now) {
      int min = now / 60;
      String text = String.format(Locale.getDefault(), "%02d:%02d", min, now - min * 60);
      mNotifBuilder.setProgress(TIMEOUT_SEC, now, false);
      mNotifBuilder.setContentText(text);

      synchronized (LIVE) {
        if (LIVE.get()) {
          mNotifMgr.notify(UNIQUE_ID, mNotifBuilder.build());
        }
      }
    }
  }

  private static class LogCallback extends ILogCallback.Stub {

    private LogcatSvc mSvc;

    private LogCallback(LogcatSvc svc) {
      mSvc = svc;
    }

    public boolean writeToLogFile(String line) {
      LogcatSvc svc = mSvc;
      return svc != null && svc.writeToLogFile(line);
    }
  }

  private SingleTaskExecutor mLogExecutor;
  private PrintWriter mLogWriter;

  private boolean writeToLogFile(String line) {
    synchronized (LIVE) {
      if (mLogWriter != null && line != null && LIVE.get()) {
        mLogWriter.println(line);
        return true;
      }
    }
    mS.stopSelf();
    return false;
  }

  public static void start(Uri logFile) {
    sendIntent(new Intent(App.getCxt(), LogcatSvcM.class).setData(logFile));
  }

  public static void stopSvc() {
    App.getCxt().stopService(new Intent(App.getCxt(), LogcatSvcM.class));
  }

  public static void sendDaemonCallback() {
    if (MySettings.INS.isDebug()) {
      sendIntent(new Intent(App.getCxt(), LogcatSvcM.class).setAction(ACTION_SEND_CALLBACK));
    }
  }

  private static void sendIntent(Intent intent) {
    if (VERSION.SDK_INT >= VERSION_CODES.O) {
      App.getCxt().startForegroundService(intent);
    } else {
      App.getCxt().startService(intent);
    }
  }
}
