package com.mirfatif.permissionmanagerx.fwk;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import androidx.appcompat.app.AlertDialog;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.base.BaseActivity;
import com.mirfatif.permissionmanagerx.pkg.PackageActivity;

public class PackageActivityM extends BaseActivity {

  public final PackageActivity mA = new PackageActivity(this);

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

  protected void onResume() {
    super.onResume();
    mA.onResume();
  }

  protected void onStart() {
    super.onStart();
    mA.onStart();
  }

  protected void onStop() {
    mA.onStop();
    super.onStop();
  }

  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    AlertDialog d = mA.createDialog(tag);
    return d != null ? d : super.createDialog(tag, dialogFragment);
  }
}
