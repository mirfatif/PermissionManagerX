package com.mirfatif.permissionmanagerx.util;

import android.app.Activity;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;

public class UiUtilsFlavor {

  private UiUtilsFlavor() {}

  public static void onCreateStart(Activity activity) {}

  public static int getAccentColor() {
    return App.getCxt().getColor(R.color.green);
  }
}
