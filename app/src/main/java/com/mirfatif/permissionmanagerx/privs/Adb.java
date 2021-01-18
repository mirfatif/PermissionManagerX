package com.mirfatif.permissionmanagerx.privs;

import android.util.Base64;
import android.util.Log;
import android.widget.Toast;
import com.cgutman.adblib.AdbAuthenticationFailedException;
import com.cgutman.adblib.AdbBase64;
import com.cgutman.adblib.AdbConnection;
import com.cgutman.adblib.AdbCrypto;
import com.cgutman.adblib.AdbStream;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.Socket;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class Adb {

  private static final String TAG = "Adb";

  private AdbCrypto adbCrypto;
  private Socket adbSocket;
  private AdbConnection adbConnection;
  private AdbStream adbStream;
  private AdbReader adbReader;
  private AdbWriter adbWriter;

  private final boolean showToastOnFailure;

  Adb(String command, boolean showToastOnFailure) throws AdbException {
    this.showToastOnFailure = showToastOnFailure;
    createConnection(command, true);
  }

  private void createConnection(String command, boolean showPrompt) throws AdbException {
    File adbDir = new File(App.getContext().getFilesDir(), "adb");
    if (!adbDir.exists() && !adbDir.mkdirs()) {
      throw new AdbException("Could not create directory: " + adbDir);
    }
    File pvtKey = new File(adbDir, "pvt.key");
    File pubKey = new File(adbDir, "pub.key");

    AdbBase64 adbBase64 = data -> Base64.encodeToString(data, Base64.NO_WRAP);
    if (pvtKey.exists() && pubKey.exists()) {
      try {
        adbCrypto = AdbCrypto.loadAdbKeyPair(adbBase64, pvtKey, pubKey);
      } catch (InvalidKeySpecException | IOException | NoSuchAlgorithmException e) {
        e.printStackTrace();
      }
    }

    try {
      if (adbCrypto == null) {
        adbCrypto = AdbCrypto.generateAdbKeyPair(adbBase64);
        adbCrypto.saveAdbKeyPair(pvtKey, pubKey);
      }

      adbSocket = new Socket("127.0.0.1", MySettings.getInstance().getAdbPort());
      adbSocket.setTcpNoDelay(true);

      adbConnection = AdbConnection.create(adbSocket, adbCrypto);
      if (!adbConnection.connect(15, TimeUnit.SECONDS, !showPrompt)) {
        if (showPrompt) {
          closeQuietly();
          // Let's do one more quick try
          createConnection(command, false);
          return;
        }
        throw new TimeoutException("ADB connection timed out");
      }

      // If command is empty, shell is opened. But default shell writes its STDIN to STDERR. So we
      // execute another shell.
      if (command.isEmpty()) {
        command = "exec sh";
      }
      Log.i(TAG, "Executing: shell:" + command);
      adbStream = adbConnection.open("shell:" + command);

    } catch (NoSuchAlgorithmException e) {
      throwAdbException(R.string.adb_key_pair_failed, e, "Creating ADB key pair failed");
    } catch (IOException e) {
      throwAdbException(R.string.adb_connect_failed, e, "ADB connection failed");
    } catch (InterruptedException e) {
      throwAdbException(R.string.adb_connect_interrupted, e, "ADB connection interrupted");
    } catch (AdbAuthenticationFailedException e) {
      throwAdbException(R.string.adb_authentication_failed, e, "ADB authentication failed");
    } catch (TimeoutException e) {
      throwAdbException(R.string.adb_connect_timed_out, e, e.getMessage());
    }
  }

  private void throwAdbException(int resId, Exception e, String msg) throws AdbException {
    e.printStackTrace();
    closeQuietly();
    if (showToastOnFailure) {
      Utils.runInFg(() -> Toast.makeText(App.getContext(), resId, Toast.LENGTH_LONG).show());
    }
    throw new AdbException(msg);
  }

  public AdbReader getReader() {
    if (adbReader == null) {
      adbReader = new AdbReader(adbStream);
    }
    return adbReader;
  }

  AdbWriter getWriter() {
    if (adbWriter == null) {
      adbWriter = new AdbWriter(adbStream);
    }
    return adbWriter;
  }

  public void close() throws IOException {
    if (adbReader != null) {
      adbReader.close();
    }
    if (adbWriter != null) {
      adbWriter.close();
    }
    if (adbStream != null) {
      adbStream.close();
    }
    if (adbConnection != null) {
      adbConnection.close();
    }
    if (adbSocket != null) {
      adbSocket.close();
    }
    adbReader = null;
    adbWriter = null;
    adbStream = null;
    adbConnection = null;
    adbSocket = null;
    adbCrypto = null;
  }

  private void closeQuietly() {
    try {
      close();
    } catch (IOException ignored) {
    }
  }

  public static boolean isConnected(boolean showToastOnFailure) {
    Adb adb;
    try {
      adb = new Adb("id -u", showToastOnFailure);
    } catch (AdbException e) {
      Log.e(TAG, e.toString());
      return false;
    }

    BufferedReader adbReader = new BufferedReader(adb.getReader());
    String line, res = null;

    try {
      while ((line = adbReader.readLine()) != null) {
        Log.i("checkAdbConnected", line);
        res = line;
      }
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      try {
        adbReader.close();
        adb.close();
      } catch (IOException ignored) {
      }
    }

    if (res != null) {
      for (String match : new String[] {"2000", "0"}) {
        if (res.trim().equals(match)) {
          return true;
        }
      }
    }
    Utils.runInFg(
        () ->
            Toast.makeText(App.getContext(), R.string.adb_command_fail, Toast.LENGTH_LONG).show());
    return false;
  }

  private static class AdbReader extends Reader {

    private final AdbStream adbStream;
    private char[] chars;
    private int pos;

    AdbReader(AdbStream adbStream) {
      this.adbStream = adbStream;
    }

    @Override
    public synchronized int read(char[] cbuf, int off, int len) throws IOException {
      if (adbStream == null) {
        throw new IOException("Stream is null");
      }
      int availableChars;
      if (chars == null || (availableChars = chars.length - pos) == 0) {
        if (adbStream.isClosed() && adbStream.isQueueEmpty()) {
          return -1;
        }
        byte[] payload;
        try {
          payload = adbStream.read();
        } catch (InterruptedException e) {
          throw new IOException(e);
        }
        if (payload == null) {
          return -1;
        }
        chars = new String(payload).toCharArray();
        pos = 0;
        availableChars = chars.length;
      }

      if (len > availableChars) {
        len = availableChars;
      }
      System.arraycopy(chars, pos, cbuf, off, len);
      pos += len;
      return len;
    }

    @Override
    public synchronized void close() throws IOException {
      if (adbStream == null) {
        throw new IOException("Stream is null");
      }
      adbStream.close();
    }
  }

  private static class AdbWriter extends Writer {

    private final AdbStream adbStream;

    AdbWriter(AdbStream adbStream) {
      this.adbStream = adbStream;
    }

    @Override
    public synchronized void write(char[] cbuf, int off, int len) throws IOException {
      if (adbStream == null) {
        throw new IOException("Stream is null");
      }
      StringBuilder stringBuilder = new StringBuilder();
      for (int i = off; i < off + len; i++) {
        stringBuilder.append(cbuf[i]);
      }
      if (stringBuilder.length() == 0) {
        return;
      }
      try {
        /** {@link AdbStream#write(String)} may append unnecessary null byte */
        // Commands should always be flushed.
        adbStream.write(stringBuilder.toString().getBytes(), true);
      } catch (InterruptedException e) {
        throw new IOException(e);
      }
    }

    @Override
    public synchronized void flush() throws IOException {
      if (adbStream == null) {
        throw new IOException("Stream is null");
      }
      adbStream.flush();
    }

    @Override
    public synchronized void close() throws IOException {
      if (adbStream == null) {
        throw new IOException("Stream is null");
      }
      flush();
      adbStream.close();
    }
  }

  private AdbOutputStream adbOutputStream;

  AdbOutputStream getOutputStream() {
    if (adbOutputStream == null) {
      adbOutputStream = new AdbOutputStream(adbStream);
    }
    return adbOutputStream;
  }

  private static class AdbOutputStream extends OutputStream {

    private final AdbStream adbStream;
    private final byte[] payload = new byte[4096]; // write() hangs with 8192
    private int count;

    AdbOutputStream(AdbStream adbStream) {
      this.adbStream = adbStream;
    }

    @Override
    public synchronized void write(int b) throws IOException {
      if (adbStream == null) {
        throw new IOException("Stream is null");
      }
      payload[count] = (byte) b;
      count++;
      if (count >= payload.length) {
        write();
      }
    }

    private synchronized void write() throws IOException {
      if (count == 0) {
        return;
      }
      byte[] bytes = new byte[count];
      System.arraycopy(payload, 0, bytes, 0, count);
      try {
        adbStream.write(bytes, true);
      } catch (InterruptedException e) {
        throw new IOException(e);
      }
      count = 0;
    }

    @Override
    public synchronized void flush() throws IOException {
      if (adbStream == null) {
        throw new IOException("Stream is null");
      }
      if (count != 0) {
        write();
      }
      adbStream.flush();
    }

    @Override
    public synchronized void close() throws IOException {
      if (adbStream == null) {
        throw new IOException("Stream is null");
      }
      flush();
      adbStream.close();
    }
  }

  static class AdbException extends Exception {

    public AdbException(String message) {
      super(message);
    }
  }
}
