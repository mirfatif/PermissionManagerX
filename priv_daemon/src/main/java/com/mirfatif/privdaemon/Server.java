package com.mirfatif.privdaemon;

import static com.mirfatif.privdaemon.DaemonLog.e;

import android.os.RemoteException;
import android.os.SystemClock;
import com.mirfatif.err.HiddenAPIsException;
import com.mirfatif.privtasks.Constants;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Inet4Address;
import java.net.ServerSocket;
import java.net.Socket;

public enum Server {
  INS;

  private static final String TAG = "Server";

  private ServerSocket server;
  private int port;

  int getPort() throws HiddenAPIsException {
    if (port == 0) {
      try {
        server = new ServerSocket(0, 0, Inet4Address.getByAddress(new byte[] {127, 0, 0, 1}));
      } catch (IOException e) {
        throw new HiddenAPIsException(e);
      }
      port = server.getLocalPort();
      BgRunner.execute(this::waitForAppCalls);
    }
    return port;
  }

  private void waitForAppCalls() {
    while (true) {
      try (Socket client = server.accept()) {
        client.setTcpNoDelay(true);
        readMessage(client);
      } catch (IOException e) {
        DaemonLog.e(TAG, "waitForAppCalls", e);
      }

      SystemClock.sleep(1000);
    }
  }

  private static void readMessage(Socket client) {
    try (BufferedReader r = new BufferedReader(new InputStreamReader(client.getInputStream()))) {
      String line;
      while ((line = r.readLine()) != null) {
        String[] codeWord;
        if (line.startsWith(Constants.CMD_CODE_WORD)
            && (codeWord = line.split(" ")).length == 2
            && isUUID(codeWord[1])) {
          Callbacks.INS.talkToApp(codeWord[1]);
        } else {
          e(TAG, "readMessage", "bad input received: " + line);
        }
      }
    } catch (IOException | RemoteException e) {
      DaemonLog.e(TAG, "readMessage", e);
    }
  }

  private static final String UUID_PATTERN =
      "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$";

  public static boolean isUUID(String uuid) {
    return uuid.matches(UUID_PATTERN);
  }
}
