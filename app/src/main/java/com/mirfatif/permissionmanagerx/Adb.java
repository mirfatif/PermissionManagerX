package com.mirfatif.permissionmanagerx;

import android.util.Base64;
import android.util.Log;
import com.cgutman.adblib.AdbBase64;
import com.cgutman.adblib.AdbConnection;
import com.cgutman.adblib.AdbCrypto;
import com.cgutman.adblib.AdbStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.net.Socket;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

class Adb {

  private AdbCrypto adbCrypto;
  private Socket adbSocket;
  private AdbConnection adbConnection;
  private AdbStream adbStream;
  private AdbReader adbReader;
  private AdbWriter adbWriter;

  Adb(String command) throws IOException {
    File adbDir = new File(App.getContext().getFilesDir(), "adb");
    if (!adbDir.exists() && !adbDir.mkdirs()) {
      throw new IOException("Could not create directory: " + adbDir);
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
      adbConnection.connect();

      String runScript = "run.sh";
      File runScriptPath = new File(App.getContext().getExternalFilesDir(null), runScript);
      if (Utils.getUserId() == 0 && Utils.extractionFails(runScript, runScriptPath)) {
        closeQuietly();
        throw new IOException("Extraction of run.sh fails");
      }

      // if command is empty, while loop in shell script reads commands from stdIn
      String cmd = "exec sh " + Utils.getOwnerFilePath(runScriptPath) + " " + command;
      adbStream = adbConnection.open("shell:" + cmd);

    } catch (NoSuchAlgorithmException | IOException | InterruptedException e) {
      e.printStackTrace();
      closeQuietly();
      throw new IOException(e);
    }
  }

  AdbReader getReader() {
    if (adbReader == null) adbReader = new AdbReader(adbStream);
    return adbReader;
  }

  AdbWriter getWriter() {
    if (adbWriter == null) adbWriter = new AdbWriter(adbStream);
    return adbWriter;
  }

  void close() throws IOException {
    if (adbReader != null) adbReader.close();
    if (adbWriter != null) adbWriter.close();
    if (adbStream != null) adbStream.close();
    if (adbConnection != null) adbConnection.close();
    if (adbSocket != null) adbSocket.close();
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

  static boolean isConnected() {
    Adb adb;
    try {
      adb = new Adb("id -u");
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    }

    BufferedReader adbReader = new BufferedReader(adb.getReader());
    String line, res = null;
    int exitCode = -1;

    try {
      while ((line = adbReader.readLine()) != null) {
        Log.i("checkAdbConnected", line);
        if (res == null) res = line;
        if (line.startsWith("EXIT_CODE:")) {
          exitCode = Integer.parseInt(line.split(":")[1]);
          break;
        }
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

    if (exitCode != 0) return false;
    for (String match : new String[] {"2000", "0"}) {
      if (res.trim().equals(match)) return true;
    }
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
    public int read(char[] cbuf, int off, int len) throws IOException {
      if (adbStream == null) throw new IOException("Stream is null");
      int availableChars;
      if (chars == null || (availableChars = chars.length - pos) == 0) {
        if (adbStream.isClosed() && adbStream.isQueueEmpty()) return -1;
        byte[] payload;
        try {
          payload = adbStream.read();
        } catch (InterruptedException e) {
          throw new IOException(e);
        }
        if (payload == null) return -1;
        chars = new String(payload).toCharArray();
        pos = 0;
        availableChars = chars.length;
      }

      if (len > availableChars) len = availableChars;
      System.arraycopy(chars, pos, cbuf, off, len);
      pos += len;
      return len;
    }

    @Override
    public void close() throws IOException {
      if (adbStream == null) throw new IOException("Stream is null");
      adbStream.close();
    }
  }

  private static class AdbWriter extends Writer {

    private final AdbStream adbStream;

    AdbWriter(AdbStream adbStream) {
      this.adbStream = adbStream;
    }

    @Override
    public void write(char[] cbuf, int off, int len) throws IOException {
      if (adbStream == null) throw new IOException("Stream is null");
      StringBuilder stringBuilder = new StringBuilder();
      for (int i = off; i < off + len; i++) {
        stringBuilder.append(cbuf[i]);
      }
      if (stringBuilder.length() == 0) return;
      try {
        adbStream.write(stringBuilder.toString());
      } catch (InterruptedException e) {
        throw new IOException(e);
      }
    }

    @Override
    public void flush() {
      // AdbStream always flushes
    }

    @Override
    public void close() throws IOException {
      if (adbStream == null) throw new IOException("Stream is null");
      adbStream.close();
    }
  }
}
