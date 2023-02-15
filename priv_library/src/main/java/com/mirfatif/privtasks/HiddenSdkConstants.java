package com.mirfatif.privtasks;

import android.app.ActivityManager;
import android.app.AppOpsManager;
import android.content.pm.PackageManager;
import android.os.Build;
import com.mirfatif.err.HiddenAPIsException;
import com.mirfatif.privtasks.bind.PermFixedFlags;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;

public enum HiddenSdkConstants {
  _NUM_OP(0),
  MODE_NAMES_SIZE(1),
  OP_NONE(2),

  OP_FLAGS_ALL(3),
  OP_RUN_IN_BACKGROUND(4),

  OP_RUN_ANY_IN_BACKGROUND(5),

  FLAG_PERMISSION_SYSTEM_FIXED(6),
  FLAG_PERMISSION_POLICY_FIXED(7),

  START_SUCCESS(8);

  private final int ordinal;

  HiddenSdkConstants(int value) {
    this.ordinal = value;
  }

  private Integer value;

  public int get() throws HiddenAPIsException {
    return get(false);
  }

  public int get(boolean wrapHiddenSdkErrors) throws HiddenAPIsException {
    if (value != null) {
      return value;
    }

    try {
      switch (ordinal) {
        case 0:
          if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            value = HiddenAPIs.getNumOps();
          } else {
            value = getStaticIntField("_NUM_OP", AppOpsManager.class);
          }
          break;
        case 1:
          value = HiddenAPIs.getOpModeNamesSize();
          break;
        case 2:
          value = getStaticIntField("OP_NONE", AppOpsManager.class);
          break;
        case 3:
          if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            value = getStaticIntField("OP_FLAGS_ALL", AppOpsManager.class);
          }
          break;
        case 4:
          value = getStaticIntField("OP_RUN_IN_BACKGROUND", AppOpsManager.class);
          break;
        case 5:
          value = getStaticIntField("OP_RUN_ANY_IN_BACKGROUND", AppOpsManager.class);
          break;
        case 6:
          value = getStaticIntField("FLAG_PERMISSION_SYSTEM_FIXED", PackageManager.class);
          break;
        case 7:
          value = getStaticIntField("FLAG_PERMISSION_POLICY_FIXED", PackageManager.class);
          break;
        case 8:
          value = getStaticIntField("START_SUCCESS", ActivityManager.class);
          break;
      }
    } catch (NoSuchFieldError | NoSuchMethodError e) {
      if (wrapHiddenSdkErrors) {

        throw new HiddenAPIsException(e);
      } else {

        throw e;
      }
    }

    if (value == null) {
      throw new HiddenAPIsException("Bad get call");
    }

    return value;
  }

  private static int getStaticIntField(String name, Class<?> cls) throws HiddenAPIsException {
    try {
      return cls.getDeclaredField(name).getInt(null);
    } catch (IllegalAccessException | NoSuchFieldException e) {
      throw new HiddenAPIsException(e);
    }
  }

  public static PermFixedFlags getPermFixedFlags() throws HiddenAPIsException {
    return new PermFixedFlags(
        FLAG_PERMISSION_SYSTEM_FIXED.get(), FLAG_PERMISSION_POLICY_FIXED.get());
  }
}
