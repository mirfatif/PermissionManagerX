package com.mirfatif.permissionmanagerx.util;

import android.app.Activity;
import android.app.Dialog;
import android.view.View;
import android.view.ViewGroup;

public class UtilsFlavor {

  private UtilsFlavor() {}

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateDialog(Dialog dialog) {}

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateStart(Activity activity) {}

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateLayout(ViewGroup viewGroup) {}

  public static void onSnackBarSwiped(View view) {
    view.setTranslationY(0);
  }

  @SuppressWarnings("UnusedDeclaration")
  public static void onSnackBarMoved(View view) {}
}
