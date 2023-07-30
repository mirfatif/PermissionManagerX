package com.mirfatif.err;

import android.os.RemoteException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;

public class ContainerException extends RemoteException {

  private final String stackTrace;

  public ContainerException(String stackTrace) {
    this.stackTrace = stackTrace;
  }

  public void printStackTrace(PrintStream s) {
    s.println(stackTrace);
  }

  public void printStackTrace(PrintWriter s) {
    s.println(stackTrace);
  }

  public static String toStackTrace(Throwable e) {
    StringWriter sw = new StringWriter();
    try (PrintWriter pw = new PrintWriter(sw)) {
      e.printStackTrace(pw);
    }
    return sw.toString();
  }
}
