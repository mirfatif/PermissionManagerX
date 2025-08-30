package com.mirfatif.permissionmanagerx.fwk;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import androidx.appcompat.app.AlertDialog;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.base.BaseActivity;
import com.mirfatif.permissionmanagerx.main.MainActivity;

public class MainActivityM extends BaseActivity {

  public final MainActivity mA = new MainActivity(this);

  protected void onCreated(Bundle savedInstanceState) {
    mA.onCreated();
  }

  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
    mA.onNewIntent(intent);
  }

  public boolean onCreateOptionsMenu(Menu menu) {
    return mA.onCreateOptionsMenu(menu);
  }

  public boolean onPrepareOptionsMenu(Menu menu) {
    return false;
  }

  public boolean onOptionsItemSelected(MenuItem item) {
    return mA.onOptionsItemSelected(item) || super.onOptionsItemSelected(item);
  }

  protected void onResume() {
    super.onResume();
    mA.onResume();
  }

  protected void onPause() {
    super.onPause();
    mA.onPause();
  }

  public void onAttachedToWindow() {
    super.onAttachedToWindow();
    mA.onAttachedToWindow();
  }

  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    AlertDialog d = mA.createDialog(tag, dialogFragment);
    return d != null ? d : super.createDialog(tag, dialogFragment);
  }
}
