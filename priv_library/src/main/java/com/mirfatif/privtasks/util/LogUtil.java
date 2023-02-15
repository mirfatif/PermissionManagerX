package com.mirfatif.privtasks.util;

import android.os.Build;
import com.mirfatif.privtasks.util.bg.SingleTaskExecutor;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.UUID;

public class LogUtil {

  private static final String TAG = "LogUtil";

  private LogUtil() {}

  public static void writeCrashLog(
      File logFile, String appState, String stackTrace, boolean isDaemon) throws IOException {
    PrintWriter writer = new PrintWriter(new FileWriter(logFile, true));
    writer.println("=================================");
    writer.println(appState);
    writer.println("Time: " + Util.getCurrDateTime(true, true));
    writer.println("Component: " + (isDaemon ? "Daemon" : "App"));
    writer.println("Log ID: " + UUID.randomUUID().toString());
    writer.println("=================================");
    writer.println(stackTrace);
    writer.close();
  }

  public static SingleTaskExecutor readLogcat(LogCallback callback) {
    String pid = String.valueOf(android.os.Process.myPid());
    String[] cmd = new String[] {"logcat", "--pid", pid};
    Process p = Util.runProc(TAG, "doLogging", true, cmd);
    if (p == null) {
      return null;
    }
    SingleTaskExecutor executor =
        new SingleTaskExecutor(
            () -> readLogcatStream(p, callback, String.join(" ", cmd)), "LogCat-Reader-PID-" + pid);
    executor.submit();
    return executor;
  }

  private static void readLogcatStream(Process process, LogCallback callback, String cmd) {
    NonBlockingReader reader = new NonBlockingReader(process.getInputStream());
    String line;
    try {
      while ((line = reader.readLine(process, 0)) != null) {
        if (!callback.onNewLine(line)) {
          break;
        }
      }
    } catch (IOException e) {
      MyLog.e(TAG, "readLogcatStream", e);
    } catch (InterruptedException e) {
      MyLog.i(TAG, "readLogcatStream", "Interrupted: [" + cmd + "]");
    }

    callback.onNewLine(null);

    try {
      process.getInputStream().close();
    } catch (Throwable ignored) {
    }

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      process.destroyForcibly();
    } else {
      process.destroy();
    }
  }

  public interface LogCallback {

    boolean onNewLine(String line);
  }
}
