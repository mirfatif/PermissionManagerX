package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.os.Bundle;
import android.view.MenuItem;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
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
  protected void onSaveInstanceState(@NonNull Bundle outState) {
    // We can't save state of AlertDialogFragment since AlertDialog is passed as a constructor
    // argument. Otherwise separate AlertDialogFragment class needs to be created for every dialog.
    AlertDialogFragment.removeAll(this);
    super.onSaveInstanceState(outState);
  }
}
