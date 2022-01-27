package com.mirfatif.permissionmanagerx.util;

import android.app.Activity;
import androidx.annotation.ColorInt;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;

public class UtilsFlavor {

  private UtilsFlavor() {}

  public static void onCreateStart(@SuppressWarnings("UnusedDeclaration") Activity activity) {}

  public static @ColorInt int getAccentColor() {
    return App.getContext().getColor(R.color.green);
  }
}
