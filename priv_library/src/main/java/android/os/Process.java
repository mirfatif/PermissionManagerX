package android.os;

import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.NonSDK;

@NonSDK
public class Process {

  @SuppressWarnings("UnusedDeclaration")
  public static int[] getPidsForCommands(String[] cmds) {
    return null;
  }

  // Not hidden API
  public static int myUid() {
    return 0;
  }
}
