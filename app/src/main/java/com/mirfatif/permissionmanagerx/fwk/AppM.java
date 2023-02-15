package com.mirfatif.permissionmanagerx.fwk;

import android.app.Application;
import android.content.res.Configuration;
import com.mirfatif.permissionmanagerx.app.App;

public class AppM extends Application {

  private final App mA = new App(this);

  public void onCreate() {
    super.onCreate();
    mA.onCreate();
  }

  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(mA.onConfigurationChanged(newConfig));
  }
}
