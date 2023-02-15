package com.mirfatif.privtasks.hiddenapis;

public class HiddenAPIsConstants {

  private HiddenAPIsConstants() {}

  static final int START_SVC_ERR = 1;
  static final int PKG_URI = 2;

  static String getString(int id, String... args) {
    switch (id) {
      case START_SVC_ERR:
        return "Could not start " + args[0];
      case PKG_URI:
        return "package:" + args[0];
      default:
        return null;
    }
  }
}
