package android.app;

import java.util.List;

public class AppOpsManager {

  public abstract static class PackageOps {

    public abstract List<OpEntry> getOps();

    public abstract String getPackageName();
  }

  public abstract static class OpEntry {

    public abstract int getOp();

    public abstract int getMode();

    public abstract long getLastAccessTime(int i);

    public abstract long getTime();
  }

  public static final String[] MODE_NAMES = null;

  public static final int MODE_ALLOWED = 0;

  public static final int MODE_IGNORED = 1;

  public static final int MODE_ERRORED = 2;

  public static final int MODE_DEFAULT = 3;

  public static final int MODE_FOREGROUND = 4;

  public static int getNumOps() {
    return 0;
  }

  public static int opToDefaultMode(int i) {
    return 0;
  }

  public static int opToDefaultMode(int i, boolean b) {
    return 0;
  }

  public static int opToSwitch(int i) {
    return 0;
  }

  public static String opToName(int i) {
    return null;
  }

  public static String modeToName(int i) {
    return null;
  }

  public static int permissionToOpCode(String s) {
    return 0;
  }

  public static int strDebugOpToOp(String s) {
    return 0;
  }
}
