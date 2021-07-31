package com.mirfatif.permissionmanagerx.util;

import static android.os.Build.VERSION.SDK_INT;
import static android.text.Spanned.SPAN_EXCLUSIVE_EXCLUSIVE;
import static android.text.style.DynamicDrawableSpan.ALIGN_BASELINE;
import static com.mirfatif.permissionmanagerx.util.UtilsFlavor.getAccentColor;

import android.app.Activity;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.ColorStateList;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Build;
import android.os.Build.VERSION_CODES;
import android.os.Handler;
import android.os.Looper;
import android.os.Parcel;
import android.os.SystemClock;
import android.text.Html;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.style.BulletSpan;
import android.text.style.ImageSpan;
import android.text.style.RelativeSizeSpan;
import android.text.style.TextAppearanceSpan;
import android.text.style.URLSpan;
import android.util.Log;
import android.util.TypedValue;
import android.widget.Button;
import android.widget.Toast;
import androidx.annotation.AttrRes;
import androidx.annotation.ColorInt;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.browser.customtabs.CustomTabColorSchemeParams;
import androidx.browser.customtabs.CustomTabsIntent;
import androidx.browser.customtabs.CustomTabsService;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.content.FileProvider;
import androidx.core.content.res.ResourcesCompat;
import androidx.preference.PreferenceManager;
import androidx.security.crypto.EncryptedSharedPreferences;
import androidx.security.crypto.EncryptedSharedPreferences.PrefKeyEncryptionScheme;
import androidx.security.crypto.EncryptedSharedPreferences.PrefValueEncryptionScheme;
import androidx.security.crypto.MasterKey;
import androidx.security.crypto.MasterKey.KeyScheme;
import com.google.android.material.color.MaterialColors;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.annot.SecurityLibBug;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.Adb;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.permissionmanagerx.svc.NotifDismissSvc;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
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

  private static final String TAG = "Utils";

  public static final int UID_SYSTEM = android.os.Process.SYSTEM_UID;
  public static final int UID_ROOT = SDK_INT >= VERSION_CODES.Q ? android.os.Process.ROOT_UID : 0;
  public static final int UID_SHELL =
      SDK_INT >= VERSION_CODES.Q ? android.os.Process.SHELL_UID : 2000;

  private Utils() {}

  private static final Handler mMainThreadHandler = new Handler(Looper.getMainLooper());

  public static void runInFg(Runnable runnable) {
    mMainThreadHandler.post(runnable);
  }

  private static final ExecutorService mExecutor = Executors.newCachedThreadPool();

  public static Future<?> runInBg(Runnable runnable) {
    return mExecutor.submit(runnable);
  }

  public static void runCommand(String tag, String... cmd) {
    Process process = runCommand(tag, true, cmd);
    if (process == null) {
      return;
    }

    try (BufferedReader stdIn =
        new BufferedReader(new InputStreamReader(process.getInputStream()))) {
      String line;
      while ((line = stdIn.readLine()) != null) {
        Log.i(tag, line);
      }
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      cleanStreams(process, null, tag);
    }
  }

  public static Process runCommand(String tag, boolean redirectStdErr, String... cmd) {
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
  public static boolean copyStream(InputStream input, OutputStream output) {
    if (input == null || output == null) {
      return false;
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
      return false;
    }
    return count <= Integer.MAX_VALUE;
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
        showToast(R.string.no_browser_installed);
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

  // Doesn't work with Application or Service context
  public static @ColorInt int getColor(Activity activity, @AttrRes int colorAttrResId) {
    return MaterialColors.getColor(activity, colorAttrResId, Color.TRANSPARENT);
  }

  public static TextAppearanceSpan getHighlight(@ColorInt int colorInt) {
    return new TextAppearanceSpan(
        null,
        Typeface.NORMAL,
        -1,
        new ColorStateList(new int[][] {new int[] {}}, new int[] {colorInt}),
        null);
  }

  public static SpannableString getHighlightString(
      String text, TextAppearanceSpan highlightSpan, boolean isCaseSensitive, String... prominent) {

    if (text == null || highlightSpan == null || prominent == null) {
      return null;
    }

    SpannableString spannable = new SpannableString(text);

    // Same Span object cannot be applied multiple times, we need to create new instances.
    Parcel parcel = Parcel.obtain();
    highlightSpan.writeToParcel(parcel, 0);

    for (String prom : prominent) {
      if (prom == null || prom.length() == 0) {
        continue;
      }
      int startPos;
      if (isCaseSensitive) {
        startPos = text.indexOf(prom);
      } else {
        startPos = text.toUpperCase().indexOf(prom.toUpperCase());
      }
      if (startPos < 0) {
        continue;
      }
      int endPos = startPos + prom.length();
      parcel.setDataPosition(0); // For read
      spannable.setSpan(
          new TextAppearanceSpan(parcel), startPos, endPos, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
    }
    parcel.recycle();
    return spannable;
  }

  public static void sendMail(Activity activity, String body) {
    Intent emailIntent = new Intent(Intent.ACTION_SENDTO).setData(Uri.parse("mailto:"));
    emailIntent.putExtra(Intent.EXTRA_EMAIL, new String[] {getString(R.string.email_address)});
    emailIntent.putExtra(Intent.EXTRA_SUBJECT, getString(R.string.app_name));
    if (body != null) {
      emailIntent.putExtra(Intent.EXTRA_TEXT, body);
    }
    try {
      activity.startActivity(emailIntent);
    } catch (ActivityNotFoundException e) {
      showToast(R.string.no_email_app_installed);
    }
  }

  public static boolean isNightMode(Activity activity) {
    int uiMode = activity.getResources().getConfiguration().uiMode;
    return (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
  }

  public static boolean setNightTheme(Activity activity) {
    if (!MySettings.getInstance().forceDarkMode()) {
      AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
      return false;
    }

    // Dark Mode applied on whole device
    if (Utils.isNightMode(activity)) {
      return false;
    }

    // Dark Mode already applied in app
    int defMode = AppCompatDelegate.getDefaultNightMode();
    if (defMode == AppCompatDelegate.MODE_NIGHT_YES) {
      return false;
    }

    AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES);
    return true;
  }

  public static Context setLocale(Context context) {
    String lang = MySettings.getInstance().getLocale();
    Locale locale;
    if (TextUtils.isEmpty(lang)) {
      locale = Resources.getSystem().getConfiguration().getLocales().get(0);
    } else {
      locale = new Locale(lang);
    }
    Locale.setDefault(locale);
    Configuration config = context.getResources().getConfiguration();
    config.setLocale(locale);
    return context.createConfigurationContext(config);
  }

  public static void cleanStreams(Process process, Adb adb, String tag) {
    try {
      if (adb != null) {
        adb.close();
      }
      if (process != null) {
        process.getInputStream().close();
        process.getErrorStream().close();
        process.getOutputStream().close();
        // Try the best to kill the process. The on reading daemon's logcat might not
        // be killed because of different UID.
        process.destroy();
        if (SDK_INT >= Build.VERSION_CODES.O) {
          process.destroyForcibly();
        }
      }
    } catch (Throwable e) {
      Log.e(tag, e.toString());
    }
  }

  // Divide by UserHandle#PER_USER_RANGE
  public static int getUserId(int uid) {
    return uid / 100000;
  }

  public static int getUserId() {
    return getUserId(android.os.Process.myUid());
  }

  private static SharedPreferences mEncPrefs;
  private static final Object ENC_PREFS_LOCK = new Object();

  @SecurityLibBug
  public static SharedPreferences getEncPrefs() {
    synchronized (ENC_PREFS_LOCK) {
      if (mEncPrefs != null) {
        return mEncPrefs;
      }

      for (int i = 0; i < 10; i++) {
        try {
          mEncPrefs =
              EncryptedSharedPreferences.create(
                  App.getContext(),
                  BuildConfig.APPLICATION_ID + "_enc_prefs",
                  new MasterKey.Builder(App.getContext())
                      .setKeyScheme(KeyScheme.AES256_GCM)
                      .build(),
                  PrefKeyEncryptionScheme.AES256_SIV,
                  PrefValueEncryptionScheme.AES256_GCM);
          return mEncPrefs;
        } catch (Exception e) {
          if (i == 9) {
            e.printStackTrace();
          } else {
            Log.e(TAG, "getEncPrefs: " + e.toString());
          }
          SystemClock.sleep(100);
        }
      }

      showToast("No Encryption");

      // TODO temp fix for https://github.com/google/tink/issues/413
      mEncPrefs = App.getContext().getSharedPreferences("_enc_prefs2", Context.MODE_PRIVATE);
      return mEncPrefs;
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
        + SDK_INT
        + "\nROM: "
        + Build.DISPLAY
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

  public static String getString(int resId, Object... args) {
    return App.getContext().getString(resId, args);
  }

  public static String getQtyString(int resId, int qty, Object... args) {
    return App.getContext().getResources().getQuantityString(resId, qty, args);
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
    return htmlToString(getString(resId));
  }

  public static Spanned htmlToString(String str) {
    Spanned spanned = Html.fromHtml(str, Html.FROM_HTML_MODE_COMPACT);

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

    parcel.recycle();

    Drawable d = ResourcesCompat.getDrawable(App.getRes(), R.drawable.link, null);
    if (d != null) {
      // DrawableCompat.setTint()
      d.setTint(getAccentColor());
      d.setBounds(0, 0, dpToPx(12), dpToPx(12));
    }

    for (URLSpan span : string.getSpans(0, string.length(), URLSpan.class)) {
      int start = string.getSpanStart(span);
      int end = string.getSpanEnd(span);
      if (!string.toString().substring(start, end).equals("LINK")) {
        continue;
      }
      string.setSpan(new ImageSpan(d, ALIGN_BASELINE), start, end, SPAN_EXCLUSIVE_EXCLUSIVE);
    }

    breakParas(string);
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

  public static void showToast(String msg) {
    if (msg != null) {
      runInFg(() -> Toast.makeText(App.getContext(), msg, Toast.LENGTH_LONG).show());
    }
  }

  public static void showToast(int resId, Object... args) {
    if (resId != 0) {
      showToast(getString(resId, args));
    }
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// LOGGING ////////////////////////////
  //////////////////////////////////////////////////////////////////

  private static long daemonDeadLogTs = 0;

  public static void logDaemonDead(String tag) {
    if (MySettings.getInstance().isDebug() || System.currentTimeMillis() - daemonDeadLogTs > 1000) {
      Log.w(tag, "Privileged daemon is not running");
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
      if (!logFile.exists()
          || logFile.length() > 512 * 1024
          || logFile.lastModified() < System.currentTimeMillis() - TimeUnit.DAYS.toMillis(90)) {
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

    String authority = BuildConfig.APP_ID + ".FileProvider";
    Uri logFileUri = FileProvider.getUriForFile(App.getContext(), authority, logFile);

    final String CHANNEL_ID = "channel_crash_report";
    final String CHANNEL_NAME = getString(R.string.channel_crash_report);
    final int UNIQUE_ID = getInteger(R.integer.channel_crash_report);

    Intent intent = new Intent(Intent.ACTION_SEND);
    intent
        .setData(logFileUri)
        .setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
        .setType("text/plain")
        .putExtra(Intent.EXTRA_EMAIL, new String[] {getString(R.string.email_address)})
        .putExtra(Intent.EXTRA_SUBJECT, getString(R.string.app_name) + " - Crash Report")
        .putExtra(Intent.EXTRA_TEXT, "Find attachment.")
        .putExtra(Intent.EXTRA_STREAM, logFileUri);

    // Adding extra information to dismiss notification after the action is tapped
    intent
        .setClass(App.getContext(), NotifDismissSvc.class)
        .putExtra(NotifDismissSvc.EXTRA_INTENT_TYPE, NotifDismissSvc.INTENT_TYPE_ACTIVITY)
        .putExtra(NotifDismissSvc.EXTRA_NOTIF_ID, UNIQUE_ID);

    PendingIntent pendingIntent =
        PendingIntent.getService(
            App.getContext(), UNIQUE_ID, intent, PendingIntent.FLAG_UPDATE_CURRENT);

    NotificationManagerCompat notificationManager =
        NotificationManagerCompat.from(App.getContext());

    NotificationChannel channel = notificationManager.getNotificationChannel(CHANNEL_ID);
    if (channel == null && SDK_INT >= VERSION_CODES.O) {
      channel =
          new NotificationChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_HIGH);
      notificationManager.createNotificationChannel(channel);
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
        .addAction(0, getString(R.string.send_report), pendingIntent)
        .setColor(getAccentColor())
        .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
        .setPriority(NotificationCompat.PRIORITY_HIGH)
        .setAutoCancel(true);

    notificationManager.notify(UNIQUE_ID, notificationBuilder.build());
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
      showToast(R.string.getting_root_fail);
    }
    return res;
  }

  public static boolean checkRoot() {
    MySettings mySettings = MySettings.getInstance();
    boolean res = NativeDaemon.rootInstance().isRunning();
    if (mySettings.isDebug()) {
      Util.debugLog(TAG, "checkRoot: getting root privileges " + (res ? "succeeded" : "failed"));
    }
    return res;
  }

  @SuppressWarnings("UnusedReturnValue")
  public static boolean checkAdbIfEnabled() {
    if (MySettings.getInstance().isAdbConnected()) {
      return checkAdb(true);
    }
    return false;
  }

  public static boolean checkAdb(boolean showToastOnFailure) {
    MySettings mySettings = MySettings.getInstance();
    boolean res = NativeDaemon.adbInstance().isRunning(showToastOnFailure);
    if (mySettings.isDebug()) {
      Util.debugLog(TAG, "checkAdb: connecting to ADB " + (res ? "succeeded" : "failed"));
    }
    return res;
  }

  public static String getSu() {
    String path = MySettings.getInstance().getSuExePath();
    return path == null ? "su" : path;
  }
}
