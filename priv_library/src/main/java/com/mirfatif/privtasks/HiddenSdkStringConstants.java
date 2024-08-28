package com.mirfatif.privtasks;

import android.companion.virtual.VirtualDeviceManager;
import com.mirfatif.err.HiddenAPIsException;

public enum HiddenSdkStringConstants {
  PERSISTENT_DEVICE_ID_DEFAULT;

  private String value;

  public String get() throws HiddenAPIsException {
    return get(false);
  }

  public String get(boolean wrapHiddenSdkErrors) throws HiddenAPIsException {
    if (value != null) {
      return value;
    }

    try {
      if (this == HiddenSdkStringConstants.PERSISTENT_DEVICE_ID_DEFAULT) {
        value = getStaticStringField("PERSISTENT_DEVICE_ID_DEFAULT", VirtualDeviceManager.class);
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

  private static String getStaticStringField(String name, Class<?> cls) throws HiddenAPIsException {
    try {
      return (String) cls.getDeclaredField(name).get(null);
    } catch (IllegalAccessException | NoSuchFieldException e) {
      throw new HiddenAPIsException(e);
    }
  }
}
