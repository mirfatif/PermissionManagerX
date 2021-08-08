package com.mirfatif.permissionmanagerx.ui;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts.CreateDocument;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog.Builder;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.ActivityAboutBinding;
import com.mirfatif.permissionmanagerx.databinding.TranslationDialogBinding;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.settings.AppUpdate;
import com.mirfatif.permissionmanagerx.svc.LogcatService;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.permissionmanagerx.util.Utils;
import me.saket.bettermovementmethod.BetterLinkMovementMethod;

public class AboutActivity extends BaseActivity {

  private final MySettings mMySettings = MySettings.getInstance();
  private ActivityAboutBinding mB;
  private ActivityResultLauncher<String> mLoggingLauncher;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    if (Utils.setNightTheme(this)) {
      return;
    }
    mB = ActivityAboutBinding.inflate(getLayoutInflater());
    setContentView(mB.getRoot());

    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(R.string.about_menu_item);
    }

    mB.version.setText(BuildConfig.VERSION_NAME);
    openWebUrl(mB.telegram, R.string.telegram_link);
    openWebUrl(mB.sourceCode, R.string.source_url);
    openWebUrl(mB.issues, R.string.issues_url);
    openWebUrl(mB.rating, R.string.play_store_url);
    mB.contact.setOnClickListener(v -> Utils.sendMail(this, null));
    setLogTitle(mMySettings.isDebug() ? R.string.stop_logging : R.string.collect_logs);
    mB.logging.setOnClickListener(v -> handleLogging());
    openWebUrl(mB.privacyPolicy, R.string.privacy_policy_link);
    mB.checkUpdate.setOnClickListener(v -> checkForUpdates());
    mB.translate.setOnClickListener(v -> showLocaleDialog());
    mB.shareApp.setOnClickListener(v -> sendShareIntent());

    mB.paidFeaturesSummary.setText(Utils.htmlToString(R.string.paid_features_summary));

    mB.paidFeatures.setOnClickListener(
        v -> {
          if (mB.paidFeaturesSummary.getMaxLines() == 1) {
            mB.paidFeaturesSummary.setMaxLines(1000);
          } else {
            mB.paidFeaturesSummary.setMaxLines(1);
          }
        });

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
    // registerForActivityResult() must be called before onStart() is called
    mLoggingLauncher = registerForActivityResult(new CreateDocument(), callback);
  }

  private void openWebUrl(View view, int linkResId) {
    view.setOnClickListener(v -> Utils.openWebUrl(this, getString(linkResId)));
  }

  private void setLogTitle(int resId) {
    mB.loggingTitle.setText(resId);
  }

  private void handleLogging() {
    if (mMySettings.isDebug()) {
      LogcatService.sendStopLogIntent();
      setLogTitle(R.string.collect_logs);
      Snackbar.make(mB.logging, R.string.logging_stopped, 5000).show();
      return;
    }

    Utils.showToast(R.string.select_log_file);
    mLoggingLauncher.launch("PermissionManagerX_" + Utils.getCurrDateTime(false) + ".log");
  }

  private boolean mCheckForUpdateInProgress = false;

  private void checkForUpdates() {
    if (mCheckForUpdateInProgress) {
      return;
    }
    mCheckForUpdateInProgress = true;

    mB.checkUpdateSummary.setText(R.string.check_in_progress);
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
            mB.checkUpdateSummary.setText(R.string.update_summary);
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
            Utils.showToast(messageResId);
          }
          mCheckForUpdateInProgress = false;
        });
  }

  private void showLocaleDialog() {
    TranslationDialogBinding b = TranslationDialogBinding.inflate(getLayoutInflater());
    b.langCreditsV.setText(Utils.htmlToString(R.string.language_credits));
    BetterLinkMovementMethod method = BetterLinkMovementMethod.newInstance();
    method.setOnLinkClickListener((tv, url) -> Utils.openWebUrl(this, url));
    b.langCreditsV.setMovementMethod(method);
    Builder builder = new Builder(this).setTitle(R.string.translations).setView(b.getRoot());
    new AlertDialogFragment(builder.create()).show(this, "LOCALE", false);
  }

  private void sendShareIntent() {
    Intent intent = new Intent(Intent.ACTION_SEND).setType("text/plain");
    intent.putExtra(Intent.EXTRA_SUBJECT, getString(R.string.app_name));
    String text = getString(R.string.share_text, getString(R.string.play_store_url));
    startActivity(Intent.createChooser(intent.putExtra(Intent.EXTRA_TEXT, text), null));
  }
}
