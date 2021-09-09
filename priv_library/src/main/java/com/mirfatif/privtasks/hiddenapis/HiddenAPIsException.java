package com.mirfatif.privtasks.hiddenapis;

/*
 Wrapper to send consistent Exceptions to calling PrivTasks class from different implementations
 of HiddenAPIs.
 It wraps any Exceptions or Errors not thrown due to methods being hidden.
*/
public class HiddenAPIsException extends Exception {

  public HiddenAPIsException(Throwable cause) {
    super(cause);
  }

  public HiddenAPIsException(String message) {
    super(message);
  }
}
