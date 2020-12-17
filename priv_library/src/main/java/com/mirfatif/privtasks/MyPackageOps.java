package com.mirfatif.privtasks;

import java.io.Serializable;
import java.util.List;

public class MyPackageOps implements Serializable {

  private static final long serialVersionUID = 1234567890L;

  String packageName;
  List<MyOpEntry> myOpEntryList;

  public String getPackageName() {
    return packageName;
  }

  public List<MyOpEntry> getOps() {
    return myOpEntryList;
  }

  public static class MyOpEntry implements Serializable {

    private static final long serialVersionUID = MyPackageOps.serialVersionUID;

    int op;
    long lastAccessTime;
    int opMode;

    public int getOp() {
      return op;
    }

    public long getLastAccessTime() {
      return lastAccessTime;
    }

    public int getMode() {
      return opMode;
    }
  }
}
