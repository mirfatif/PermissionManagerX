package com.mirfatif.privtasks.util;

import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.Signature;
import android.os.Build;
import android.os.Parcel;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;
import java.util.stream.Collectors;

public class Util {

  private Util() {}

  public static Process runProc(String tag, String method, boolean redirectStdErr, String... cmd) {
    ProcessBuilder processBuilder = new ProcessBuilder(cmd);
    processBuilder.redirectErrorStream(redirectStdErr);

    MyLog.i(tag, method, "Executing: " + Arrays.toString(cmd));
    try {
      return processBuilder.start();
    } catch (IOException e) {
      MyLog.e(tag, method, e);
      return null;
    }
  }

  public static String getCurrDateTime(boolean spaced, boolean utc) {
    String pattern = spaced ? "dd-MMM-yy HH:mm:ss" : "dd-MMM-yy_HH-mm-ss";
    SimpleDateFormat sdf = new SimpleDateFormat(pattern, Locale.ENGLISH);
    if (utc) {
      sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
    }
    return sdf.format(System.currentTimeMillis());
  }

  public static String readNullTermFile(String path) {
    try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
      String line = reader.readLine();
      return line == null ? null : line.replaceAll("\0", " ").trim();
    } catch (IOException ignored) {
      return null;
    }
  }

  public static int[] getArray(List<Integer> list) {
    return list.parallelStream().mapToInt(Integer::intValue).toArray();
  }

  public static List<Integer> getList(int[] array) {
    return Arrays.stream(array).boxed().collect(Collectors.toList());
  }

  public static <T extends Serializable> T readSerializable(Parcel parcel, Class<T> cls) {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
      return parcel.readSerializable(cls.getClassLoader(), cls);
    } else {
      return cls.cast(parcel.readSerializable());
    }
  }

  public static final int PM_GET_SIGNATURES = buildPmSignFlag();

  private static int buildPmSignFlag() {
    return Build.VERSION.SDK_INT >= Build.VERSION_CODES.P
        ? PackageManager.GET_SIGNING_CERTIFICATES
        : PackageManager.GET_SIGNATURES;
  }

  public static Signature[] getPackageSignatures(PackageInfo pkgInfo) {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      return pkgInfo.signingInfo == null
          ? new Signature[0]
          : pkgInfo.signingInfo.getApkContentsSigners();
    } else {
      return pkgInfo.signatures;
    }
  }

  public static boolean isFwkPkg(PackageInfo packageInfo, List<Integer> systemSignatures) {
    for (Signature signature : getPackageSignatures(packageInfo)) {
      if (systemSignatures.contains(signature.hashCode())) {
        return true;
      }
    }
    return false;
  }

  public static void printStackTrace(String tag, String method) {
    StackTraceElement[] elements = Thread.currentThread().getStackTrace();
    StringWriter sw = new StringWriter();
    PrintWriter writer = new PrintWriter(sw, true);
    writer.println();
    for (int i = 3; i < elements.length; i++) {
      StackTraceElement s = elements[i];
      writer.println(
          "\tat "
              + s.getClassName()
              + "."
              + s.getMethodName()
              + "("
              + s.getFileName()
              + ":"
              + s.getLineNumber()
              + ")");
    }
    MyLog.e(tag, method, sw.toString());
  }
}
