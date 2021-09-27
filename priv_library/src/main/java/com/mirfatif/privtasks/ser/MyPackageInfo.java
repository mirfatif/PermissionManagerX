package com.mirfatif.privtasks.ser;

import java.io.Serializable;

public class MyPackageInfo implements Serializable {

  private static final long serialVersionUID = 1234567890L;

  public String packageName;

  public int[] requestedPermissionsFlags;

  public int uid;

  public boolean enabled;
}
