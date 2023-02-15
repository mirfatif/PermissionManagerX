package com.mirfatif.err;

import android.os.RemoteException;

public class HiddenAPIsException extends RemoteException {

  private static final long serialVersionUID = 1234567890L;

  public HiddenAPIsException(Throwable cause) {
    this(cause.getMessage());
    initCause(cause);
  }

  public HiddenAPIsException(String message) {
    super(message);
  }
}
