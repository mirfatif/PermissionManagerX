package com.mirfatif.privtasks;

import android.app.ActivityManager;
import android.app.AppOpsManager;
import android.content.pm.PackageManager;
import android.os.Build;
import com.mirfatif.err.HiddenAPIsException;
import com.mirfatif.privtasks.bind.PermFixedFlags;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs;

public enum HiddenSdkIntConstants {
  _NUM_OP,
  MODE_NAMES_SIZE,
  OP_NONE,
  OP_FLAGS_ALL,
  OP_RUN_IN_BACKGROUND,
  OP_RUN_ANY_IN_BACKGROUND,

  FLAG_PERMISSION_SYSTEM_FIXED,
  FLAG_PERMISSION_POLICY_FIXED,

  START_SUCCESS;

  private Integer value;

  public int get() throws HiddenAPIsException {
    return get(false);
  }

  public int get(boolean wrapHiddenSdkErrors) throws HiddenAPIsException {
    if (value != null) {
      return value;
    }

    try {
      switch (this) {
        case _NUM_OP -> {
          if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            value = HiddenAPIs.getNumOps();
          } else {
            value = getStaticIntField("_NUM_OP", AppOpsManager.class);
          }
        }
        case MODE_NAMES_SIZE -> value = HiddenAPIs.getOpModeNamesSize();
        case OP_NONE -> value = getStaticIntField("OP_NONE", AppOpsManager.class);
        case OP_FLAGS_ALL -> {
          if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            value = getStaticIntField("OP_FLAGS_ALL", AppOpsManager.class);
          }
        }
        case OP_RUN_IN_BACKGROUND ->
            value = getStaticIntField("OP_RUN_IN_BACKGROUND", AppOpsManager.class);
        case OP_RUN_ANY_IN_BACKGROUND ->
            value = getStaticIntField("OP_RUN_ANY_IN_BACKGROUND", AppOpsManager.class);
        case FLAG_PERMISSION_SYSTEM_FIXED ->
            value = getStaticIntField("FLAG_PERMISSION_SYSTEM_FIXED", PackageManager.class);
        case FLAG_PERMISSION_POLICY_FIXED ->
            value = getStaticIntField("FLAG_PERMISSION_POLICY_FIXED", PackageManager.class);
        case START_SUCCESS -> value = getStaticIntField("START_SUCCESS", ActivityManager.class);
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
