package com.mirfatif.permissionmanagerx.prefs.settings;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.net.Uri;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.util.Log;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationCompat.Builder;
import androidx.core.app.NotificationManagerCompat;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import org.json.JSONException;
import org.json.JSONObject;

public class AppUpdate {

  private static final String TAG = "AppUpdate";
  private static final String CHECK_URL =
      "https://api.github.com/repos/mirfatif/PermissionManagerX/releases/latest";
  private static final String DOWNLOAD_URL =
      "https://github.com/mirfatif/PermissionManagerX/releases/latest";
  private static final String VERSION_TAG = "tag_name";
  private String mVersion, mUpdateUrl;

  public Boolean check(boolean notify) {
    MySettings mySettings = MySettings.getInstance();

    if (notify && !mySettings.shouldCheckForUpdates()) {
      return null;
    }

    HttpURLConnection connection = null;
    InputStream inputStream = null;
    try {
      connection = (HttpURLConnection) new URL(CHECK_URL).openConnection();
      connection.setConnectTimeout(60000);
      connection.setReadTimeout(60000);
      connection.setUseCaches(false);

      int status = connection.getResponseCode();
      if (status != HttpURLConnection.HTTP_OK) {
        Log.e(TAG, connection.getResponseCode() + ": " + connection.getResponseMessage());
        return null;
      }

      inputStream = connection.getInputStream();
      BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
      StringBuilder builder = new StringBuilder();
      String line;
      while ((line = reader.readLine()) != null) {
        builder.append(line);
      }

      mVersion = new JSONObject(builder.toString()).getString(VERSION_TAG);
      // Convert tag name to version code (v1.01-beta to 101)
      int version = Integer.parseInt(mVersion.substring(1, 5).replace(".", ""));
      if (version <= BuildConfig.VERSION_CODE) {
        Log.i(TAG, "App is up-to-date");
        return false;
      } else {
        Log.i(TAG, "New update is available: " + mVersion);
        if (!BuildConfig.GH_VERSION) {
          mUpdateUrl = Utils.getString(R.string.play_store_url);
        } else {
          mUpdateUrl = DOWNLOAD_URL;
        }
        if (notify) {
          showNotification();
          mySettings.setCheckForUpdatesTs(System.currentTimeMillis());
        }
        return true;
      }
    } catch (IOException | JSONException | NumberFormatException e) {
      Log.e(TAG, e.toString());
      return null;
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

  private void showNotification() {
    final String CHANNEL_ID = "channel_app_update";
    final String CHANNEL_NAME = Utils.getString(R.string.channel_app_update);
    final int UNIQUE_ID = Utils.getInteger(R.integer.channel_app_update);

    PendingIntent pendingIntent =
        PendingIntent.getActivity(
            App.getContext(),
            UNIQUE_ID,
            new Intent(Intent.ACTION_VIEW, Uri.parse(mUpdateUrl)),
            PendingIntent.FLAG_UPDATE_CURRENT);

    NotificationManagerCompat mNotificationManager =
        NotificationManagerCompat.from(App.getContext());

    NotificationChannel channel = mNotificationManager.getNotificationChannel(CHANNEL_ID);
    if (channel == null && VERSION.SDK_INT >= VERSION_CODES.O) {
      channel =
          new NotificationChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_HIGH);
      mNotificationManager.createNotificationChannel(channel);
    }

    Builder notificationBuilder = new Builder(App.getContext(), CHANNEL_ID);
    notificationBuilder
        .setSmallIcon(R.drawable.notification_icon)
        .setColor(Utils.getAccentColor())
        .setContentTitle(Utils.getString(R.string.new_version_available))
        .setContentText(Utils.getString(R.string.tap_to_download) + " " + mVersion)
        .setContentIntent(pendingIntent)
        .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
        .setPriority(NotificationCompat.PRIORITY_HIGH)
        .setAutoCancel(true);

    mNotificationManager.notify(UNIQUE_ID, notificationBuilder.build());
  }

  public String getVersion() {
    return mVersion;
  }

  public String getUpdateUrl() {
    return mUpdateUrl;
  }
}
