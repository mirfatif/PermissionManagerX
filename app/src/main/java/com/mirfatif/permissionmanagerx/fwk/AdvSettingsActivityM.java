package com.mirfatif.permissionmanagerx.fwk;

import android.os.Bundle;
import com.mirfatif.permissionmanagerx.base.BaseActivity;
import com.mirfatif.permissionmanagerx.prefs.settings.AdvSettingsActivity;

public class AdvSettingsActivityM extends BaseActivity {

  public final AdvSettingsActivity mA;

  public AdvSettingsActivityM() {
    mA = new AdvSettingsActivity(this);
  }

  protected void onCreated(Bundle savedInstanceState) {
    mA.onCreate(savedInstanceState);
  }
}
