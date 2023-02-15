package com.mirfatif.permissionmanagerx.fwk;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import com.mirfatif.permissionmanagerx.base.BaseActivity;
import com.mirfatif.permissionmanagerx.help.HelpActivity;

public class HelpActivityM extends BaseActivity {

  private final HelpActivity mA = new HelpActivity(this);

  protected void onCreated(Bundle savedInstanceState) {
    mA.onCreated();
  }

  public void onBackPressed() {
    if (!mA.onBackPressed()) {
      super.onBackPressed();
    }
  }

  public boolean onCreateOptionsMenu(Menu menu) {
    return mA.onCreateOptionsMenu(menu);
  }

  public boolean onPrepareOptionsMenu(Menu menu) {
    return mA.onPrepareOptionsMenu(menu);
  }

  public boolean onOptionsItemSelected(MenuItem item) {
    return mA.onOptionsItemSelected(item) || super.onOptionsItemSelected(item);
  }
}
