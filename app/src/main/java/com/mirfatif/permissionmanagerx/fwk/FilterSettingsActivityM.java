package com.mirfatif.permissionmanagerx.fwk;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import androidx.appcompat.app.AlertDialog;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.base.BaseActivity;
import com.mirfatif.permissionmanagerx.prefs.settings.FilterSettingsActivity;

public class FilterSettingsActivityM extends BaseActivity {

  public final FilterSettingsActivity mA = new FilterSettingsActivity(this);

  protected void onCreated(Bundle savedInstanceState) {
    mA.onCreated(savedInstanceState);
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

  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    AlertDialog d = mA.createDialog(tag);
    return d != null ? d : super.createDialog(tag, dialogFragment);
  }
}
