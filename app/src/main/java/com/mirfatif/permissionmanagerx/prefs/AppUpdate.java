package com.mirfatif.permissionmanagerx.prefs;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.net.Uri;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationCompat.Builder;
import androidx.core.app.NotificationManagerCompat;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.NotifUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.privtasks.util.MyLog;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import org.json.JSONArray;
import org.json.JSONException;

public class AppUpdate {

  private static final String TAG = "AppUpdate";

  private static final String URL =
      "https://api.github.com/repos/mirfatif/PermissionManagerX/releases";
  private static final String NAME_FIELD = "name";
  private static final String FOSS_SUFFIX = "-foss";

  private AppUpdate() {}

  public static AppUpdateResult check(boolean notify) {
    if (notify && !MySettings.INS.shouldCheckForUpdates()) {
      return null;
    }

    boolean checkFailed = false;

    try {
      String oldVerStr = BuildConfig.VERSION_NAME;
      int oldVer = getVersion(oldVerStr);

      String newVerStr = getLatestFossVersion();
      int newVer = getVersion(newVerStr);

      boolean oldIsBeta = oldVerStr.contains("-beta");
      boolean newIsBeta = newVerStr.contains("-beta");

      if (newVer > oldVer + 1
          || (newVer > oldVer && (!newIsBeta || (newIsBeta && oldIsBeta)))
          || (newVer == oldVer
              && ((oldIsBeta && !newIsBeta)
                  || (oldIsBeta
                      && newIsBeta
                      && getBetaSubVersion(newVerStr) > getBetaSubVersion(oldVerStr))))) {
        MyLog.i(TAG, "check", "New update is available: " + oldVerStr + " -> " + newVerStr);
      } else if (newVer > oldVer && !oldIsBeta && newIsBeta) {
        MyLog.i(TAG, "check", "New update is available: " + oldVerStr + " -> " + newVerStr);
        if (notify) {
          return null;
        }
      } else {
        MyLog.i(TAG, "check", "App is up-to-date: " + oldVerStr + " -> " + newVerStr);
        return null;
      }

      if (notify && ApiUtils.hasNotifPerm()) {
        showNotification(newVerStr);
      }

      return new AppUpdateResult(false, newVerStr);
    } catch (IOException | JSONException | NumberFormatException e) {
      MyLog.e(TAG, "check", e.toString());
      checkFailed = true;
      return new AppUpdateResult(true, null);
    } finally {
      if (notify && !checkFailed) {
        MySettings.INS.setCheckForUpdatesTs(System.currentTimeMillis());
      }
    }
  }

  private static int getVersion(String version) throws NumberFormatException {
    return Integer.parseInt(version.substring(1, 5).replace(".", ""));
  }

  private static int getBetaSubVersion(String version) throws NumberFormatException {
    version = version.replaceFirst(".*-beta", "");
    for (int i = 0; i < version.length(); i++) {
      char c = version.charAt(i);
      if (c < '0' || c > '9') {
        version = version.substring(0, i);
        break;
      }
    }
    return Integer.parseInt(version);
  }

  private static String getLatestFossVersion() throws IOException, JSONException {
    HttpURLConnection connection = null;
    InputStream inputStream = null;
    try {
      connection = (HttpURLConnection) new URL(URL).openConnection();
      connection.setConnectTimeout(60000);
      connection.setReadTimeout(60000);
      connection.setUseCaches(false);

      int status = connection.getResponseCode();
      if (status != HttpURLConnection.HTTP_OK) {
        throw new IOException(
            "Response code: "
                + connection.getResponseCode()
                + ", msg: "
                + connection.getResponseMessage());
      }

      inputStream = connection.getInputStream();
      BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
      StringBuilder builder = new StringBuilder();
      String line;
      while ((line = reader.readLine()) != null) {
        builder.append(line);
      }

      var json = new JSONArray(builder.toString());
      String name;
      for (int i = 0; i < json.length(); i++) {
        name = json.getJSONObject(i).getString(NAME_FIELD);
        if (name.endsWith(FOSS_SUFFIX)) {
          return name;
        }
      }
      throw new JSONException("No foss release found");
    } finally {
      try {
        if (inputStream != null) {
          inputStream.close();
        }
      } catch (IOException ignored) {
      }
      if (connection != null) {
        connection.disconnect();
      }
    }
  }

  private static void showNotification(String version) {
    final String CHANNEL_ID = "channel_app_update";
    final String CHANNEL_NAME = getString(R.string.channel_app_update);
    final int UNIQUE_ID = ApiUtils.getInt(R.integer.channel_app_update);

    Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(getString(R.string.source_url)));
    PendingIntent pi =
        PendingIntent.getActivity(App.getCxt(), UNIQUE_ID, intent, NotifUtils.PI_FLAGS);

    NotificationManagerCompat mNotificationManager = NotificationManagerCompat.from(App.getCxt());

    NotificationChannel channel = mNotificationManager.getNotificationChannel(CHANNEL_ID);
    if (channel == null && VERSION.SDK_INT >= VERSION_CODES.O) {
      channel =
          new NotificationChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_HIGH);
      mNotificationManager.createNotificationChannel(channel);
    }

    Builder notificationBuilder = new Builder(App.getCxt(), CHANNEL_ID);
    notificationBuilder
        .setSmallIcon(R.drawable.notification_icon)
        .setColor(UiUtils.getAccentColor())
        .setContentTitle(getString(R.string.new_version_available))
        .setContentText(getString(R.string.tap_to_download) + " " + version)
        .setContentIntent(pi)
        .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
        .setPriority(NotificationCompat.PRIORITY_HIGH)
        .setAutoCancel(true)
        .addAction(0, getString(R.string.download), pi);

    NotifUtils.notify(UNIQUE_ID, notificationBuilder.build());
  }

  public static class AppUpdateResult {

    public final boolean failed;
    @Nullable public final String version;

    private AppUpdateResult(boolean failed, @Nullable String version) {
      this.failed = failed;
      this.version = version;
    }
  }
}
