package com.mirfatif.permissionmanagerx;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.util.Log;
import android.widget.Toast;
import androidx.browser.customtabs.CustomTabColorSchemeParams;
import androidx.browser.customtabs.CustomTabsIntent;
import androidx.browser.customtabs.CustomTabsService;
import androidx.lifecycle.MutableLiveData;
import com.mirfatif.privdaemon.PrivDaemon;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

class Utils {

  private Utils() {}

  private static final MySettings mMySettings = MySettings.getInstance();

  private static Handler mMainThreadHandler;

  static void runInFg(Runnable runnable) {
    if (mMainThreadHandler == null) mMainThreadHandler = new Handler(Looper.getMainLooper());
    mMainThreadHandler.post(runnable);
  }

  private static ExecutorService mExecutor;

  static Future<?> runInBg(Runnable runnable) {
    if (mExecutor == null) {
      mExecutor = Executors.newCachedThreadPool();
    }
    return mExecutor.submit(runnable);
  }

  private static ExecutorService mUpdatePackagesExecutor;

  static Future<?> updatePackagesExecutor(Runnable runnable) {
    if (mUpdatePackagesExecutor == null) {
      mUpdatePackagesExecutor = Executors.newSingleThreadExecutor();
    }
    return mUpdatePackagesExecutor.submit(runnable);
  }

  private static ExecutorService mSearchQueryExecutor;

  static Future<?> searchQueryExecutor(Runnable runnable) {
    if (mSearchQueryExecutor == null) {
      mSearchQueryExecutor = Executors.newSingleThreadExecutor();
    }
    return mSearchQueryExecutor.submit(runnable);
  }

  static boolean runCommand(String command, String tag, String match) {
    Process process = runCommand(command, tag, true);
    if (process == null) return false;
    try (BufferedReader stdIn =
        new BufferedReader(new InputStreamReader(process.getInputStream()))) {
      String line;
      String res = "";
      if (match != null) {
        res = stdIn.readLine();
        if (!TextUtils.isEmpty(res)) Log.i(tag, res);
      }
      while ((line = stdIn.readLine()) != null) Log.i(tag, line);

      if (process.waitFor() != 0) return false;
      if (match == null || res.trim().equals(match)) return true;
    } catch (IOException | InterruptedException e) {
      Log.e(tag, e.toString());
    }
    return false;
  }

  static Process runCommand(String cmd, String tag, boolean redirectStdErr) {
    File binDir = new File(App.getContext().getFilesDir(), "bin");
    File cwd = App.getContext().getExternalFilesDir(null);

    ProcessBuilder processBuilder = new ProcessBuilder(cmd.split(" "));
    processBuilder.directory(cwd);
    Map<String, String> env = processBuilder.environment();
    env.put("PATH", binDir + ":" + env.get("PATH"));
    processBuilder.redirectErrorStream(redirectStdErr);

    Log.i(tag, "Executing: " + cmd);
    try {
      return processBuilder.start();
    } catch (IOException e) {
      e.printStackTrace();
      return null;
    }
  }

  static boolean extractBinary() {
    File binDir = new File(App.getContext().getFilesDir(), "bin");
    String binary = "set_priv";
    File file = new File(binDir, binary);
    if (file.exists()) return true;

    if (!binDir.exists() && !binDir.mkdirs()) {
      Log.e("extractBinary", "Could not create directory " + binDir);
      return false;
    }

    long lastUpdated = new File(App.getContext().getApplicationInfo().sourceDir).lastModified();
    if (lastUpdated < file.lastModified()) return true;

    String arch = "_arm";
    String supportedABIs = Arrays.toString(Build.SUPPORTED_ABIS).toLowerCase();
    if (supportedABIs.contains("x86")) {
      arch = "_x86";
    } else if (!supportedABIs.contains("arm")) {
      Log.e("extractBinary", "Arch not supported " + supportedABIs);
      return false;
    }

    try (InputStream inputStream = App.getContext().getAssets().open(binary + arch);
        OutputStream outputStream = new FileOutputStream(file)) {
      if (!(copyStream(inputStream, outputStream))) {
        return false;
      }
      String command = "chmod 0755 " + file;
      Process p = Runtime.getRuntime().exec(command);
      if (p.waitFor() != 0) {
        Log.e("extractBinary", command + " failed");
        return false;
      }
    } catch (IOException | InterruptedException e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  static boolean extractionFails(String fileName, File filePath) {
    long lastUpdated = new File(App.getContext().getApplicationInfo().sourceDir).lastModified();
    InputStream inputStream = null;
    FileOutputStream outputStream = null;
    try {
      if (lastUpdated > filePath.lastModified()) {
        inputStream = App.getContext().getAssets().open(fileName);
        outputStream = new FileOutputStream(filePath);
        if (!(Utils.copyStream(inputStream, outputStream))) return true;
        outputStream.flush();
      }
    } catch (IOException e) {
      e.printStackTrace();
      return true;
    } finally {
      try {
        if (outputStream != null) outputStream.close();
        if (inputStream != null) inputStream.close();
      } catch (IOException ignored) {
      }
    }
    return false;
  }

  static final int INT_FIELD_ERROR = -1;

  static int getIntField(String name, Class<?> cls, String tag) {
    try {
      Field idField = cls.getDeclaredField(name);
      return idField.getInt(idField);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      Log.e(tag, e.toString());
      return INT_FIELD_ERROR;
    }
  }

  // org.apache.commons.io.IOUtils.copy()
  static boolean copyStream(InputStream input, OutputStream output) {
    if (input == null || output == null) return false;
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

  // org.apache.commons.lang3.StringUtils.capitalize()
  static String capitalizeString(String str) {
    final int strLen = str.length();
    if (strLen == 0) return str;

    str = str.toLowerCase();

    final int firstCodepoint = str.codePointAt(0);
    final int newCodePoint = Character.toTitleCase(firstCodepoint);
    if (firstCodepoint == newCodePoint) return str;

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

  static void openWebUrl(Activity activity, String url) {
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
      return;
    }

    CustomTabColorSchemeParams colorSchemeParams =
        new CustomTabColorSchemeParams.Builder()
            .setToolbarColor(
                App.getContext().getResources().getColor(R.color.accentTransparent, null))
            .build();

    CustomTabsIntent customTabsIntent =
        new CustomTabsIntent.Builder()
            .setShareState(CustomTabsIntent.SHARE_STATE_ON)
            .setDefaultColorSchemeParams(colorSchemeParams)
            .build();

    customTabsIntent.launchUrl(activity, Uri.parse(url));
  }

  static boolean isNightMode(Activity activity) {
    int uiMode = activity.getResources().getConfiguration().uiMode;
    return (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
  }

  static final MutableLiveData<Boolean> mHiddenAPIsNotWorking = new MutableLiveData<>(false);

  static void hiddenAPIsNotWorking(String tag, String error) {
    Log.e(tag, error);
    mMySettings.mHiddenAPIsWorking = false;
    runInFg(() -> mHiddenAPIsNotWorking.setValue(true));
    runInFg(() -> mHiddenAPIsNotWorking.setValue(false)); // for using again
  }

  static void cleanProcess(BufferedReader reader, Process process, Adb adb, String tag) {
    // Try best to kill the process. The on reading daemon's logcat might not
    // be killed because of different UID.
    try {
      if (reader != null) reader.close();
      if (adb != null) adb.close();
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

  static String getCurrDateTime() {
    return new SimpleDateFormat("dd-MMM-yy_HH-mm-ss", Locale.ENGLISH)
        .format(System.currentTimeMillis());
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// LOGGING ////////////////////////////
  //////////////////////////////////////////////////////////////////

  static boolean doLoggingFails(String[] command) {
    Process process = runCommand(command[0], "Logging", true);
    if (process == null) return true;

    runInBg(() -> readLogcatStream(process, null));

    if (command.length > 1) {
      PrintWriter writer = new PrintWriter(process.getOutputStream());
      Log.i("Logging", "Sending command to shell: " + command[1]);
      writer.println(command[1]);
      writer.flush();
    }
    return false;
  }

  static void readLogcatStream(Process process, Adb adb) {
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
      while ((line = reader.readLine()) != null && !line.contains(PrivDaemon.STOP_LOGGING)) {
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

  static BufferedWriter mLogcatWriter;

  private static void writeToLogFile(String line) throws IOException {
    if (!mMySettings.DEBUG) return;
    synchronized (Utils.class) {
      mLogcatWriter.write(line);
      mLogcatWriter.newLine();
    }
  }

  static void startLoggingTimer() {
    Executors.newSingleThreadScheduledExecutor().schedule(Utils::stopLogging, 5, TimeUnit.MINUTES);
  }

  static synchronized void stopLogging() {
    if (!mMySettings.DEBUG) return;
    mMySettings.doLogging = false;
    mMySettings.DEBUG = false;
    try {
      if (mLogcatWriter != null) mLogcatWriter.close();
    } catch (IOException ignored) {
    }
    if (mMySettings.mPrivDaemonAlive) {
      PrivDaemonHandler.getInstance().sendRequest(PrivDaemon.STOP_LOGGING);
    }
    Log.i("stopLogging()", PrivDaemon.STOP_LOGGING);
  }

  static void debugLog(String tag, String message) {
    Log.d(tag, message + " - " + System.nanoTime());
  }

  private static long daemonDeadLogTs = 0;

  static void logDaemonDead(String tag) {
    if (mMySettings.DEBUG || System.currentTimeMillis() - daemonDeadLogTs > 1000) {
      Log.e(tag, "Privileged daemon is not running");
      daemonDeadLogTs = System.currentTimeMillis();
    }
  }
}

interface PkgClickListener {
  void onClick(Package pkg);
}

interface PkgLongClickListener {
  void onLongClick(Package pkg);
}

interface PermClickListener {
  void onClick(Permission permission);
}

interface PermClickListenerWithLoc {
  void onClick(Permission permission, int yLocation);
}

interface PermSpinnerSelectListener {
  void onSelect(Permission permission, int selectedValue);
}

interface PermLongClickListener {
  void onLongClick(Permission permission);
}
