package com.mirfatif.permissionmanagerx;

import android.app.Activity;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ActivityNotFoundException;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.graphics.Color;
import android.net.Uri;
import android.os.Build;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.Handler;
import android.os.Looper;
import android.os.Parcel;
import android.text.Html;
import android.text.Spannable;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.style.BulletSpan;
import android.text.style.RelativeSizeSpan;
import android.util.Log;
import android.util.TypedValue;
import android.widget.Button;
import android.widget.Toast;
import androidx.annotation.AttrRes;
import androidx.annotation.ColorInt;
import androidx.appcompat.app.AlertDialog;
import androidx.browser.customtabs.CustomTabColorSchemeParams;
import androidx.browser.customtabs.CustomTabsIntent;
import androidx.browser.customtabs.CustomTabsService;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.content.FileProvider;
import androidx.preference.PreferenceManager;
import androidx.security.crypto.EncryptedSharedPreferences;
import androidx.security.crypto.EncryptedSharedPreferences.PrefKeyEncryptionScheme;
import androidx.security.crypto.EncryptedSharedPreferences.PrefValueEncryptionScheme;
import androidx.security.crypto.MasterKey;
import androidx.security.crypto.MasterKey.KeyScheme;
import com.google.android.material.color.MaterialColors;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.Adb;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.security.GeneralSecurityException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Utils {

  private Utils() {}

  private static final Handler mMainThreadHandler = new Handler(Looper.getMainLooper());

  public static void runInFg(Runnable runnable) {
    mMainThreadHandler.post(runnable);
  }

  private static final ExecutorService mExecutor = Executors.newCachedThreadPool();

  public static Future<?> runInBg(Runnable runnable) {
    return mExecutor.submit(runnable);
  }

  public static boolean runCommand(String[] command, String tag, String match) {
    Process process = runCommand(command, tag, true);
    if (process == null) {
      return false;
    }
    try (BufferedReader stdIn =
        new BufferedReader(new InputStreamReader(process.getInputStream()))) {
      String line;
      String res = "";
      if (match != null) {
        res = stdIn.readLine();
        if (!TextUtils.isEmpty(res)) {
          Log.i(tag, res);
        }
      }
      while ((line = stdIn.readLine()) != null) {
        Log.i(tag, line);
      }

      if (process.waitFor() != 0) {
        return false;
      }
      if (match == null || res.trim().equals(match)) {
        return true;
      }
    } catch (IOException | InterruptedException e) {
      Log.e(tag, e.toString());
    }
    return false;
  }

  public static Process runCommand(String[] cmd, String tag, boolean redirectStdErr) {
    File binDir = new File(App.getContext().getFilesDir(), "bin");
    File cwd = App.getContext().getExternalFilesDir(null);

    ProcessBuilder processBuilder = new ProcessBuilder(cmd);
    processBuilder.directory(cwd);
    Map<String, String> env = processBuilder.environment();
    env.put("PATH", binDir + ":" + env.get("PATH"));
    processBuilder.redirectErrorStream(redirectStdErr);

    Log.i(tag, "Executing: " + Arrays.toString(cmd));
    try {
      return processBuilder.start();
    } catch (IOException e) {
      e.printStackTrace();
      return null;
    }
  }

  public static void readProcessLog(BufferedReader reader, String tag) throws IOException {
    StringWriter crashLogWriter = null;
    String line;
    while ((line = reader.readLine()) != null) {
      if (line.contains(Commands.CRASH_LOG_STARTS)) {
        crashLogWriter = new StringWriter();
        continue;
      }
      if (crashLogWriter != null) {
        crashLogWriter.append(line).append("\n");
      }
      Log.e(tag, line);
    }
    if (crashLogWriter != null) {
      writeCrashLog(crashLogWriter.toString(), true);
    }
  }

  public static Integer getStaticIntField(String name, Class<?> cls, String tag) {
    try {
      return cls.getDeclaredField(name).getInt(null);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      Log.e(tag, e.toString());
      return null;
    }
  }

  // org.apache.commons.io.IOUtils.copy()
  public static boolean copyStreamFails(InputStream input, OutputStream output) {
    if (input == null || output == null) {
      return true;
    }
    byte[] buffer = new byte[8192];
    int len;
    long count = 0;
    try {
      while ((len = input.read(buffer)) != -1) {
        output.write(buffer, 0, len);
        count += len;
      }
    } catch (IOException e) {
      e.printStackTrace();
      return true;
    }
    return count > Integer.MAX_VALUE;
  }

  public static String capitalizeWords(String str) {
    StringBuilder stringBuilder = new StringBuilder();
    for (String word : str.split(" ")) {
      if (stringBuilder.length() != 0) {
        stringBuilder.append(" ");
      }
      stringBuilder.append(capitalizeString(word));
    }
    return stringBuilder.toString();
  }

  // org.apache.commons.lang3.StringUtils.capitalize()
  public static String capitalizeString(String str) {
    final int strLen = str.length();
    if (strLen == 0) {
      return str;
    }

    str = str.toLowerCase();

    final int firstCodepoint = str.codePointAt(0);
    final int newCodePoint = Character.toTitleCase(firstCodepoint);
    if (firstCodepoint == newCodePoint) {
      return str;
    }

    final int[] newCodePoints = new int[strLen];
    int outOffset = 0;
    newCodePoints[outOffset++] = newCodePoint;
    for (int inOffset = Character.charCount(firstCodepoint); inOffset < strLen; ) {
      final int codepoint = str.codePointAt(inOffset);
      newCodePoints[outOffset++] = codepoint;
      inOffset += Character.charCount(codepoint);
    }
    return new String(newCodePoints, 0, outOffset);
  }

  public static String ellipsize(String str, int len) {
    if (str == null || str.length() <= len) {
      return str;
    }
    return str.substring(0, len - 3).concat("…");
  }

  public static boolean openWebUrl(Activity activity, String url) {
    List<ResolveInfo> customTabsServices =
        App.getContext()
            .getPackageManager()
            .queryIntentServices(
                new Intent(CustomTabsService.ACTION_CUSTOM_TABS_CONNECTION),
                PackageManager.MATCH_ALL);

    if (customTabsServices.isEmpty()) {
      try {
        activity.startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(url)));
      } catch (ActivityNotFoundException e) {
        Toast.makeText(App.getContext(), R.string.no_browser_installed, Toast.LENGTH_LONG).show();
      }
      return true;
    }

    CustomTabColorSchemeParams colorSchemeParams =
        new CustomTabColorSchemeParams.Builder()
            .setToolbarColor(getColor(activity, R.attr.accentTransColor))
            .build();

    CustomTabsIntent customTabsIntent =
        new CustomTabsIntent.Builder()
            .setShareState(CustomTabsIntent.SHARE_STATE_ON)
            .setDefaultColorSchemeParams(colorSchemeParams)
            .build();

    customTabsIntent.launchUrl(activity, Uri.parse(url));

    return true;
  }

  // Doesn't work with app context
  public static @ColorInt int getColor(Activity activity, @AttrRes int colorAttrResId) {
    return MaterialColors.getColor(activity, colorAttrResId, Color.TRANSPARENT);
  }

  public static boolean sendMail(Activity activity, String body) {
    Intent emailIntent = new Intent(Intent.ACTION_SENDTO).setData(Uri.parse("mailto:"));
    emailIntent.putExtra(Intent.EXTRA_EMAIL, new String[] {getString(R.string.email_address)});
    emailIntent.putExtra(Intent.EXTRA_SUBJECT, getString(R.string.app_name));
    if (body != null) {
      emailIntent.putExtra(Intent.EXTRA_TEXT, body);
    }
    try {
      activity.startActivity(emailIntent);
    } catch (ActivityNotFoundException e) {
      Toast.makeText(App.getContext(), R.string.no_email_app_installed, Toast.LENGTH_LONG).show();
    }
    return true;
  }

  public static boolean isNightMode(Activity activity) {
    int uiMode = activity.getResources().getConfiguration().uiMode;
    return (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
  }

  public static void cleanProcess(BufferedReader reader, Process process, Adb adb, String tag) {
    // Try best to kill the process. The on reading daemon's logcat might not
    // be killed because of different UID.
    try {
      if (reader != null) {
        reader.close();
      }
      if (adb != null) {
        adb.close();
      }
      if (process != null) {
        process.getInputStream().close();
        process.getErrorStream().close();
        process.getOutputStream().close();
        process.destroy();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
          process.destroyForcibly();
        }
      }
    } catch (Throwable e) {
      Log.e(tag, e.toString());
    }
  }

  public static int getUserId() {
    return android.os.Process.myUid() / 100000;
  }

  public static SharedPreferences getEncPrefs() {
    try {
      return EncryptedSharedPreferences.create(
          App.getContext(),
          BuildConfig.APPLICATION_ID + "_enc_prefs",
          new MasterKey.Builder(App.getContext()).setKeyScheme(KeyScheme.AES256_GCM).build(),
          PrefKeyEncryptionScheme.AES256_SIV,
          PrefValueEncryptionScheme.AES256_GCM);
    } catch (GeneralSecurityException | IOException e) {
      e.printStackTrace();
      throw new RuntimeException(e);
    }
  }

  public static SharedPreferences getDefPrefs() {
    return PreferenceManager.getDefaultSharedPreferences(App.getContext());
  }

  public static String getCurrDateTime(boolean spaced) {
    if (spaced) {
      return new SimpleDateFormat("dd-MMM-yy HH:mm:ss", Locale.ENGLISH)
          .format(System.currentTimeMillis());
    } else {
      return new SimpleDateFormat("dd-MMM-yy_HH-mm-ss", Locale.ENGLISH)
          .format(System.currentTimeMillis());
    }
  }

  public static String getDeviceInfo() {
    MySettings mySettings = MySettings.getInstance();
    return "Version: "
        + BuildConfig.VERSION_NAME
        + (BuildConfig.GH_VERSION ? "" : " PlayStore")
        + "\nSDK: "
        + VERSION.SDK_INT
        + "\nBuild: "
        + Build.TYPE
        + "\nDevice: "
        + Build.DEVICE
        + "\nManufacturer: "
        + Build.MANUFACTURER
        + "\nModel: "
        + Build.MODEL
        + "\nProduct: "
        + Build.PRODUCT
        + "\nRoot: "
        + mySettings.isRootGranted()
        + "\nADB: "
        + mySettings.isAdbConnected();
  }

  public static String getString(int resId) {
    return App.getContext().getString(resId);
  }

  public static int getInteger(int resId) {
    return App.getContext().getResources().getInteger(resId);
  }

  // With longer button text, unnecessary bottom padding is added to dialog.
  public static void removeButtonPadding(AlertDialog dialog) {
    dialog.setOnShowListener(
        d -> {
          Button b = dialog.getButton(DialogInterface.BUTTON_NEUTRAL);
          int padding = dpToPx(4);
          b.setPadding(b.getPaddingLeft(), padding, padding, padding);
        });
  }

  public static Spanned htmlToString(int resId) {
    Spanned spanned = Html.fromHtml(getString(resId), Html.FROM_HTML_MODE_COMPACT);

    // Let's customize BulletSpans
    SpannableStringBuilder string = new SpannableStringBuilder(spanned);

    Parcel parcel = Parcel.obtain();
    parcel.writeInt(dpToPx(4)); // gapWidth
    parcel.writeInt(0); // wantColor
    parcel.writeInt(0); // color
    parcel.writeInt(dpToPx(2)); // bulletRadius

    for (BulletSpan span : string.getSpans(0, string.length(), BulletSpan.class)) {
      int start = string.getSpanStart(span);
      int end = string.getSpanEnd(span);
      string.removeSpan(span);
      parcel.setDataPosition(0); // For read
      string.setSpan(new BulletSpan(parcel), start, end, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
    }

    breakParas(string);
    parcel.recycle();
    return string;
  }

  public static SpannableStringBuilder breakParas(String string) {
    return breakParas(new SpannableStringBuilder(string));
  }

  public static SpannableStringBuilder breakParas(SpannableStringBuilder string) {
    // Remove newLine chars at end
    while (true) {
      int len = string.length();
      if (string.charAt(len - 1) != '\n') {
        break;
      }
      string.delete(len - 1, len);
    }

    Matcher matcher = Pattern.compile("\n").matcher(string);
    int from = 0;
    while (matcher.find(from)) {
      // Replace the existing newLine char with 2 newLine chars
      string.replace(matcher.start(), matcher.end(), "\n\n");
      // On next iteration skip the newly added newLine char
      from = matcher.end() + 1;

      // Add span to the newly added newLine char
      string.setSpan(
          new RelativeSizeSpan(0.25f),
          matcher.start() + 1,
          matcher.end() + 1,
          Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

      // On Android 7 Matcher is not refreshed if the string is changed
      matcher = Pattern.compile("\n").matcher(string);
    }

    return string;
  }

  public static int dpToPx(float dp) {
    return (int)
        TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP, dp, Resources.getSystem().getDisplayMetrics());
  }

  public static String colorIntToRGB(@ColorInt int color, boolean retainAlpha) {
    if (retainAlpha) {
      return String.format("#%08X", color);
    } else {
      return String.format("#%06X", 0xFFFFFF & color);
    }
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// LOGGING ////////////////////////////
  //////////////////////////////////////////////////////////////////

  public static boolean doLoggingFails(String[] command) {
    if (command.length != 2) {
      Log.e("Logging", "Command array length must be 2");
      return true;
    }

    try {
      writeToLogFile(getDeviceInfo());
    } catch (IOException e) {
      e.printStackTrace();
    }

    Process process = runCommand(new String[] {command[0]}, "Logging", true);
    if (process == null) {
      return true;
    }

    runInBg(() -> readLogcatStream(process, null));

    PrintWriter writer = new PrintWriter(process.getOutputStream());
    Log.i("Logging", "Sending command to shell: " + command[1]);
    writer.println(command[1]);
    writer.flush();

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
      Log.e("readLogcatStream", e.toString());
    } finally {
      // if process exited itself
      stopLogging();
      cleanProcess(reader, process, adb, "readLogcatStream");
    }
  }

  public static BufferedWriter mLogcatWriter;
  private static final Object LOG_WRITER_LOCK = new Object();

  private static void writeToLogFile(String line) throws IOException {
    synchronized (LOG_WRITER_LOCK) {
      if (!MySettings.getInstance().isDebug()) {
        return;
      }
      mLogcatWriter.write(line);
      mLogcatWriter.newLine();
    }
  }

  public static void startLoggingTimer() {
    Executors.newSingleThreadScheduledExecutor().schedule(Utils::stopLogging, 5, TimeUnit.MINUTES);
  }

  public static void stopLogging() {
    synchronized (LOG_WRITER_LOCK) {
      MySettings mySettings = MySettings.getInstance();
      if (!mySettings.isDebug()) {
        return;
      }
      mySettings.setLogging(false);
      try {
        if (mLogcatWriter != null) {
          mLogcatWriter.close();
        }
      } catch (IOException ignored) {
      }
      if (mySettings.isPrivDaemonAlive()) {
        PrivDaemonHandler.getInstance().sendRequest(Commands.STOP_LOGGING);
      }
      Log.i("stopLogging()", Commands.STOP_LOGGING);
    }
  }

  private static long daemonDeadLogTs = 0;

  public static void logDaemonDead(String tag) {
    if (MySettings.getInstance().isDebug() || System.currentTimeMillis() - daemonDeadLogTs > 1000) {
      Log.e(tag, "Privileged daemon is not running");
      daemonDeadLogTs = System.currentTimeMillis();
    }
  }

  private static final Object CRASH_LOG_LOCK = new Object();

  public static void writeCrashLog(String stackTrace, boolean isDaemon) {
    synchronized (CRASH_LOG_LOCK) {
      // Be ashamed of your performance, don't ask for feedback in near future
      MySettings.getInstance().setAskForFeedbackTs(System.currentTimeMillis());

      File logFile = new File(App.getContext().getExternalFilesDir(null), "PMX_crash.log");
      boolean append = true;
      if (!logFile.exists() || logFile.length() > 512 * 1024) {
        append = false;
      }
      try {
        PrintWriter writer = new PrintWriter(new FileWriter(logFile, append));
        writer.println("=================================");
        writer.println(getDeviceInfo());
        writer.println("Time: " + getCurrDateTime(true));
        writer.println("Component: " + (isDaemon ? "Daemon" : "App"));
        writer.println("Log ID: " + UUID.randomUUID().toString());
        writer.println("=================================");
        writer.println(stackTrace);
        writer.close();
        showCrashNotification(logFile);
      } catch (IOException ignored) {
      }
    }
  }

  private static void showCrashNotification(File logFile) {
    if (!MySettings.getInstance().shouldAskToSendCrashReport()) {
      return;
    }

    String authority = BuildConfig.APPLICATION_ID + ".FileProvider";
    Uri logFileUri = FileProvider.getUriForFile(App.getContext(), authority, logFile);

    Intent intent = new Intent(Intent.ACTION_SEND);
    intent
        .setData(logFileUri)
        .setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
        .setType("text/plain")
        .putExtra(Intent.EXTRA_EMAIL, new String[] {getString(R.string.email_address)})
        .putExtra(Intent.EXTRA_SUBJECT, getString(R.string.app_name) + " - Crash Report")
        .putExtra(Intent.EXTRA_TEXT, "Find attachment.")
        .putExtra(Intent.EXTRA_STREAM, logFileUri);

    final String CHANNEL_ID = "channel_crash_report";
    final String CHANNEL_NAME = getString(R.string.channel_crash_report);
    final int UNIQUE_ID = getInteger(R.integer.channel_crash_report);

    PendingIntent pendingIntent =
        PendingIntent.getActivity(
            App.getContext(), UNIQUE_ID, intent, PendingIntent.FLAG_UPDATE_CURRENT);

    NotificationManagerCompat mNotificationManager =
        NotificationManagerCompat.from(App.getContext());

    NotificationChannel channel = mNotificationManager.getNotificationChannel(CHANNEL_ID);
    if (channel == null && VERSION.SDK_INT >= VERSION_CODES.O) {
      channel =
          new NotificationChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_HIGH);
      mNotificationManager.createNotificationChannel(channel);
    }

    NotificationCompat.Builder notificationBuilder =
        new NotificationCompat.Builder(App.getContext(), CHANNEL_ID);
    notificationBuilder
        .setSmallIcon(R.drawable.notification_icon)
        .setContentTitle(getString(R.string.crash_report))
        .setContentText(getString(R.string.ask_to_report_crash_small))
        .setStyle(
            new NotificationCompat.BigTextStyle().bigText(getString(R.string.ask_to_report_crash)))
        .setContentIntent(pendingIntent)
        .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
        .setPriority(NotificationCompat.PRIORITY_HIGH)
        .setAutoCancel(true);

    mNotificationManager.notify(UNIQUE_ID, notificationBuilder.build());
  }

  //////////////////////////////////////////////////////////////////
  /////////////////////////// PRIVILEGES ///////////////////////////
  //////////////////////////////////////////////////////////////////

  @SuppressWarnings("UnusedReturnValue")
  public static boolean checkRootIfEnabled() {
    if (!MySettings.getInstance().isRootGranted()) {
      return false;
    }
    boolean res = checkRoot();
    if (!res) {
      runInFg(
          () ->
              Toast.makeText(App.getContext(), R.string.getting_root_fail, Toast.LENGTH_LONG)
                  .show());
    }
    return res;
  }

  public static boolean checkRoot() {
    MySettings mySettings = MySettings.getInstance();
    boolean res = runCommand(new String[] {"su", "-c", "id  -u"}, "checkRoot", "0");
    mySettings.setRootGranted(res);
    if (mySettings.isDebug()) {
      Util.debugLog("checkRoot", "Getting root privileges " + (res ? "succeeded" : "failed"));
    }
    return res;
  }

  @SuppressWarnings("UnusedReturnValue")
  public static boolean checkAdbIfEnabled() {
    if (MySettings.getInstance().isAdbConnected()) {
      return checkAdb();
    }
    return false;
  }

  public static boolean checkAdb() {
    MySettings mySettings = MySettings.getInstance();
    boolean res = Adb.isConnected();
    mySettings.setAdbConnected(res);
    if (mySettings.isDebug()) {
      Util.debugLog("checkAdb", "Connecting to ADB " + (res ? "succeeded" : "failed"));
    }
    return res;
  }
}
