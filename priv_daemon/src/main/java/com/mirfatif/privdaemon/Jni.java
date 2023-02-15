package com.mirfatif.privdaemon;

import com.mirfatif.err.HiddenAPIsException;

public enum Jni {
  INS;

  private boolean mLoaded = false;

  public void loadLib(String libPath) throws HiddenAPIsException {
    if (!mLoaded) {
      try {
        System.load(libPath);
        mLoaded = true;
      } catch (SecurityException | UnsatisfiedLinkError e) {
        throw new HiddenAPIsException(e);
      }
    }
  }

  public native boolean sendStdErr(int errPort);

  public native void closeStdErr();

  public native boolean matches(String string, String regex, String logTag);
}
