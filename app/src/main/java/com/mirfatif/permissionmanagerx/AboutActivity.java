package com.mirfatif.permissionmanagerx;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.MenuItem;
import android.widget.TextView;
import android.widget.Toast;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.privdaemon.PrivDaemon;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

public class AboutActivity extends AppCompatActivity {

  private MySettings mMySettings;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_about);

    if (getSupportActionBar() != null) {
      getSupportActionBar().setTitle(R.string.about_menu_item);
    }

    mMySettings = MySettings.getInstance();

    ((TextView) findViewById(R.id.version)).setText(BuildConfig.VERSION_NAME);

    setLogTitle();

    findViewById(R.id.contact).setOnClickListener(v -> Utils.sendMail(this, null));

    findViewById(R.id.source_code)
        .setOnClickListener(v -> Utils.openWebUrl(this, getString(R.string.source_url)));
    findViewById(R.id.issues)
        .setOnClickListener(v -> Utils.openWebUrl(this, getString(R.string.issues_url)));

    findViewById(R.id.logging).setOnClickListener(v -> handleLogging());
    findViewById(R.id.rating)
        .setOnClickListener(v -> Utils.openWebUrl(this, getString(R.string.play_store_url)));
  }

  private boolean logInProgress = false;

  private void setLogTitle() {
    logInProgress = mMySettings.DEBUG;
    ((TextView) findViewById(R.id.logging_title))
        .setText(logInProgress ? R.string.stop_logging : R.string.collect_logs);
  }

  private void handleLogging() {
    if (logInProgress) {
      Utils.runInBg(
          () -> {
            Utils.stopLogging();
            setLogTitle();
            Snackbar.make(findViewById(R.id.logging), R.string.logging_stopped, 5000).show();
          });
      return;
    }

    Toast.makeText(App.getContext(), R.string.select_log_file, Toast.LENGTH_LONG).show();
    ActivityResultCallback<Uri> callback = uri -> Utils.runInBg(() -> doLoggingInBg(uri));
    registerForActivityResult(new ActivityResultContracts.CreateDocument(), callback)
        .launch("PermissionManagerX_" + Utils.getCurrDateTime() + ".log");
  }

  private void doLoggingInBg(Uri uri) {
    try {
      OutputStream outStream = getApplication().getContentResolver().openOutputStream(uri, "rw");
      Utils.mLogcatWriter = new BufferedWriter(new OutputStreamWriter(outStream));
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      return;
    }

    if (mMySettings.mPrivDaemonAlive) {
      PrivDaemonHandler.getInstance().sendRequest(PrivDaemon.SHUTDOWN);
    }
    mMySettings.doLogging = true;
    mMySettings.DEBUG = true;
    Utils.runCommand("logcat -c", "Logging", null);
    Intent intent = new Intent(App.getContext(), MainActivity.class);
    intent.setAction(MainActivity.START_LOGGING).setFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
    Utils.runInFg(
        () -> {
          startActivity(intent);
          finish();
        });
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    // do not recreate parent (Main) activity
    if (item.getItemId() == android.R.id.home) {
      onBackPressed();
      return true;
    }
    return super.onOptionsItemSelected(item);
  }
}
