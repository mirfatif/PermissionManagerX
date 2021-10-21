package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.content.res.Configuration;
import android.os.Bundle;
import android.view.MenuItem;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.permissionmanagerx.util.UtilsFlavor;

public class BaseActivity extends AppCompatActivity {

  @Override
  protected void onCreate(@Nullable Bundle savedInstanceState) {
    UtilsFlavor.onCreateStart(this);
    super.onCreate(savedInstanceState);
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    // Do not recreate parent (Main) activity
    if (item.getItemId() == android.R.id.home) {
      onBackPressed();
      return true;
    }
    return super.onOptionsItemSelected(item);
  }

  @Override
  protected void attachBaseContext(Context context) {
    super.attachBaseContext(Utils.setLocale(context));
  }

  @Override
  public void onConfigurationChanged(@NonNull Configuration newConfig) {
    super.onConfigurationChanged(Utils.setLocale(newConfig));
  }

  @Override
  public void applyOverrideConfiguration(Configuration overrideConfiguration) {
    super.applyOverrideConfiguration(Utils.setLocale(overrideConfiguration));
  }

  /*
   We use the same AlertDialogFragment to show all AlertDialogs. This method
   is called to recreate the AlertDialog after configuration change.
  */
  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    return null;
  }
}
