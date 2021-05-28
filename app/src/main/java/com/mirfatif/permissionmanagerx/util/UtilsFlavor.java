package com.mirfatif.permissionmanagerx.util;

import android.app.Activity;
import android.app.Dialog;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.ColorInt;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;

public class UtilsFlavor {

  private UtilsFlavor() {}

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateDialog(Dialog dialog) {}

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateStart(Activity activity) {}

  @SuppressWarnings("UnusedDeclaration")
  public static void onCreateLayout(ViewGroup viewGroup) {}

  public static @ColorInt int getAccentColor() {
    return App.getContext().getColor(R.color.green);
  }

  public static void onSnackBarSwiped(View view) {
    view.setTranslationY(0);
  }

  @SuppressWarnings("UnusedDeclaration")
  public static void onSnackBarMoved(View view) {}
}
