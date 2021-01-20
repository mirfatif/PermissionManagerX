package com.mirfatif.permissionmanagerx.ui;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.widget.TextView;
import android.widget.Toast;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog.Builder;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.settings.AppUpdate;
import com.mirfatif.permissionmanagerx.svc.LogcatService;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;

public class AboutActivity extends BaseActivity {

  private MySettings mMySettings;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_about);

    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(R.string.about_menu_item);
    }

    mMySettings = MySettings.getInstance();

    ((TextView) findViewById(R.id.version)).setText(BuildConfig.VERSION_NAME);
    openWebUrl(R.id.telegram, R.string.telegram_link);
    openWebUrl(R.id.source_code, R.string.source_url);
    openWebUrl(R.id.issues, R.string.issues_url);
    openWebUrl(R.id.rating, R.string.play_store_url);
    findViewById(R.id.contact).setOnClickListener(v -> Utils.sendMail(this, null));
    setLogTitle(mMySettings.isDebug() ? R.string.stop_logging : R.string.collect_logs);
    findViewById(R.id.logging).setOnClickListener(v -> handleLogging());
    findViewById((R.id.check_update)).setOnClickListener(v -> checkForUpdates());

    TextView paidFeaturesView = findViewById(R.id.paid_features_summary);
    paidFeaturesView.setText(Utils.htmlToString(R.string.paid_features_summary));

    findViewById(R.id.paid_features)
        .setOnClickListener(
            v -> {
              if (paidFeaturesView.getMaxLines() == 1) {
                paidFeaturesView.setMaxLines(1000);
              } else {
                paidFeaturesView.setMaxLines(1);
              }
            });
  }

  private void openWebUrl(int viewResId, int linkResId) {
    findViewById(viewResId).setOnClickListener(v -> Utils.openWebUrl(this, getString(linkResId)));
  }

  private void setLogTitle(int resId) {
    ((TextView) findViewById(R.id.logging_title)).setText(resId);
  }

  private void handleLogging() {
    if (mMySettings.isDebug()) {
      LogcatService.sendStopLogIntent();
      setLogTitle(R.string.collect_logs);
      Snackbar.make(findViewById(R.id.logging), R.string.logging_stopped, 5000).show();
      return;
    }

    Toast.makeText(App.getContext(), R.string.select_log_file, Toast.LENGTH_LONG).show();
    ActivityResultCallback<Uri> callback =
        logFile -> {
          if (logFile != null) {
            startService(
                new Intent(
                    LogcatService.ACTION_START_LOG,
                    logFile,
                    App.getContext(),
                    LogcatService.class));
          }
        };
    registerForActivityResult(new ActivityResultContracts.CreateDocument(), callback)
        .launch("PermissionManagerX_" + Utils.getCurrDateTime(false) + ".log");
  }

  private boolean mCheckForUpdateInProgress = false;
  private TextView mCheckUpdatesSummary;

  private void checkForUpdates() {
    if (mCheckForUpdateInProgress) {
      return;
    }
    mCheckForUpdateInProgress = true;

    mCheckUpdatesSummary = findViewById(R.id.check_update_summary);
    mCheckUpdatesSummary.setText(R.string.check_in_progress);
    Utils.runInBg(this::checkForUpdatesInBg);
  }

  private void checkForUpdatesInBg() {
    AppUpdate appUpdate = new AppUpdate();
    Boolean res = appUpdate.check(false);

    int messageResId;
    boolean showDialog = false;
    if (res == null) {
      messageResId = R.string.check_for_updates_failed;
    } else if (!res) {
      messageResId = R.string.app_is_up_to_date;
    } else {
      messageResId = R.string.new_version_available;
      showDialog = true;
    }

    boolean finalShowDialog = showDialog && !isFinishing();
    Utils.runInFg(
        () -> {
          if (!isFinishing()) {
            mCheckUpdatesSummary.setText(R.string.update_summary);
          }

          if (finalShowDialog) {
            Builder builder =
                new Builder(this)
                    .setTitle(R.string.update)
                    .setMessage(Utils.getString(messageResId) + ": " + appUpdate.getVersion())
                    .setPositiveButton(
                        R.string.download,
                        (dialog, which) ->
                            Utils.runInFg(() -> Utils.openWebUrl(this, appUpdate.getUpdateUrl())))
                    .setNegativeButton(android.R.string.cancel, null);
            new AlertDialogFragment(builder.create()).show(this, "APP_UPDATE", false);
          } else {
            Toast.makeText(App.getContext(), messageResId, Toast.LENGTH_LONG).show();
          }
          mCheckForUpdateInProgress = false;
        });
  }

  @Override
  protected void onSaveInstanceState(@NonNull Bundle outState) {
    AlertDialogFragment.removeAll(this);
    super.onSaveInstanceState(outState);
  }
}
