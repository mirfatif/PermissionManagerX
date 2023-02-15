package com.mirfatif.permissionmanagerx.fwk;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import androidx.appcompat.app.AlertDialog;
import com.mirfatif.permissionmanagerx.about.AboutActivity;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.base.BaseActivity;

public class AboutActivityM extends BaseActivity {

  private final AboutActivity mA = new AboutActivity(this);

  protected void onCreated(Bundle savedInstanceState) {
    mA.onCreated();
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

  protected void onDestroy() {
    mA.onDestroy();
    super.onDestroy();
  }

  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    AlertDialog d = mA.createDialog(tag, dialogFragment);
    return d != null ? d : super.createDialog(tag, dialogFragment);
  }
}
