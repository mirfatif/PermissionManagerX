package com.mirfatif.permissionmanagerx.fwk;

import android.os.Bundle;
import com.mirfatif.permissionmanagerx.about.CrashReportActivity;
import com.mirfatif.permissionmanagerx.base.BaseActivity;

public class CrashReportActivityM extends BaseActivity {

  private final CrashReportActivity mA = new CrashReportActivity(this);

  protected void onCreated(Bundle savedInstanceState) {
    mA.onCreated();
  }
}
