package com.mirfatif.permissionmanagerx.privs;

import static com.mirfatif.permissionmanagerx.BuildConfig.DAEMON_DEX;

import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.util.MyLog;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class NativeDaemonFlavor {

  private NativeDaemonFlavor() {}

  private static final String TAG = "NativeDaemonFlavor";

  private static final String CMD_SAVE_FILE = "save_file ";

  public static boolean extractDex(String path, NativeDaemon daemon, OutputStream os) {
    try (InputStream inStream1 = App.getCxt().getAssets().open(DAEMON_DEX);
        InputStream inStream2 = App.getCxt().getAssets().open(DAEMON_DEX)) {
      int size = 0;
      while (inStream1.read() != -1) {
        size++;
      }

      daemon.sendCmd(CMD_SAVE_FILE + size + " " + path, null);

      if (!Utils.copyStream(TAG, inStream2, os)) {
        MyLog.e(TAG, "extractDex", "Extracting " + DAEMON_DEX + " failed");
        return false;
      }
      os.flush();
    } catch (IOException e) {
      MyLog.e(TAG, "extractDex", e);
      return false;
    }

    return true;
  }
}
