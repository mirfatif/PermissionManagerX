package android.app;

import java.util.List;

@SuppressWarnings("UnusedDeclaration")
public class AppOpsManager {

  public abstract static class PackageOps {

    public abstract List<OpEntry> getOps();

    public abstract String getPackageName();
  }

  public abstract static class OpEntry {

    public abstract int getOp();

    public abstract int getMode();

    public abstract long getLastAccessTime(int flags);

    public abstract long getTime();
  }

  public static final String[] MODE_NAMES = new String[] {};

  public static int getNumOps() {
    return 0;
  }

  public static int opToDefaultMode(int op) {
    return 0;
  }

  public static int opToSwitch(int op) {
    return 0;
  }

  public static String opToName(int op) {
    return null;
  }

  public static String modeToName(int mode) {
    return null;
  }

  public static int permissionToOpCode(String permission) {
    return 0;
  }

  public static int strDebugOpToOp(String op) {
    return 0;
  }
}
