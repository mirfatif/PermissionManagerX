package com.mirfatif.permissionmanagerx.parser;

public class PkgParserFlavor {

  private static PkgParserFlavor mPkgParserFlavor;

  public static synchronized PkgParserFlavor getInstance() {
    if (mPkgParserFlavor == null) {
      mPkgParserFlavor = new PkgParserFlavor();
    }
    return mPkgParserFlavor;
  }

  private PkgParserFlavor() {}

  @SuppressWarnings("UnusedDeclaration")
  void onPkgCreated(Package pkg) {}

  void onPkgListCompleted() {}
}
