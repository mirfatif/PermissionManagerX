package com.mirfatif.permissionmanagerx.util;

import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.Inet4Address;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class StdErrLogServer extends ServerSocket {

  private static final String TAG = "StdErrLogServer";

  private final Future<?> mConnection;
  private final String mTag;
  private final StreamClosedCallback mCallback;

  public StdErrLogServer(String tag, StreamClosedCallback callback) throws IOException {
    super(0, 0, Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}));
    setSoTimeout(5000);

    mConnection = BgRunner.submit(new ConnWaiter());

    mTag = tag;
    mCallback = callback;
  }

  private class ConnWaiter implements Callable<Void> {

    public Void call() throws Exception {
      Socket client = accept();
      client.setTcpNoDelay(true);
      InputStream is = client.getInputStream();
      BgRunner.execute(() -> readStdErr(is));
      close();
      return null;
    }
  }

  public void waitForConn() throws ExecutionException, InterruptedException {
    mConnection.get();
  }

  private void readStdErr(InputStream errStream) {
    BufferedReader reader = new BufferedReader(new InputStreamReader(errStream));
    String line;
    try {
      while ((line = reader.readLine()) != null) {
        MyLog.e(mTag, null, "STDERR: " + line);
      }
    } catch (IOException e) {
      MyLog.e(TAG, "readStdErr", e);
    }
    if (mCallback != null) {
      mCallback.onClose();
    }
  }

  public interface StreamClosedCallback {

    void onClose();
  }
}
