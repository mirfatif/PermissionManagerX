package com.mirfatif.privtasks.hiddenapis;

/*
Wrapper to send consistent Errors to calling PrivTasks class from different implementations of HiddenAPIs.
It wraps any Exceptions or Errors thrown due to methods being hidden which include
IllegalAccessException, NoSuchFieldException, NoSuchFieldError, NoSuchMethodException and NoSuchMethodError.
https://developer.android.com/distribute/best-practices/develop/restrictions-non-sdk-interfaces#results-of-keeping-non-sdk
*/
public class HiddenAPIsError extends Error {

  public HiddenAPIsError(Throwable cause) {
    super(cause);
  }
}
