package com.mirfatif.privtasks.util;

import static java.lang.System.currentTimeMillis;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

public class NonBlockingReader extends InputStreamReader {

  private static final String TAG = "NonBlockingReader";

  public NonBlockingReader(InputStream inputStream) {
    super(inputStream);
  }

  private final char[] mBuffer = new char[8192];
  private final Queue<String> mLines = new ConcurrentLinkedQueue<>();
  private final StringBuilder mLine = new StringBuilder();

  public String readLine(long msTimeout) throws IOException, InterruptedException {
    synchronized (this) {
      return readLine(null, msTimeout);
    }
  }

  public String readLine(Process process, long msTimeout) throws IOException, InterruptedException {
    synchronized (this) {
      long ts = Long.MAX_VALUE, sleep;
      if (msTimeout > 0) {
        ts = currentTimeMillis() + msTimeout;
      }

      while ((ts - currentTimeMillis()) > 0) {
        if (!mLines.isEmpty()) {
          return mLines.poll();
        }

        if (Thread.interrupted()) {
          throw new InterruptedException();
        }

        while (!ready() && (sleep = ts - currentTimeMillis()) > 0) {
          this.wait(Math.min(sleep, 500));

          if (process != null) {
            try {
              MyLog.e(TAG, "readLine", "Process exited with code: " + process.exitValue());
              break;
            } catch (IllegalThreadStateException ignored) {
            }
          }
        }

        if (!ready()) {
          break;
        }

        int len = read(mBuffer);
        if (len == -1) {
          break;
        }

        for (int i = 0; i < len; i++) {
          char c = mBuffer[i];
          if (c == '\n') {
            mLines.add(mLine.toString());
            mLine.setLength(0);
          } else {
            mLine.append(c);
          }
        }
      }
      return null;
    }
  }
}
