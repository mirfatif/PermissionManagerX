package com.mirfatif.permissionmanagerx.ui;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts.CreateDocument;
import androidx.annotation.DrawableRes;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.AboutPrivilegesDialogBinding;
import com.mirfatif.permissionmanagerx.databinding.ActivityAboutBinding;
import com.mirfatif.permissionmanagerx.databinding.TranslationDialogBinding;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.settings.AppUpdate;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.permissionmanagerx.svc.LogcatService;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.PermStatus;
import java.util.ArrayList;
import java.util.List;
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

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    getMenuInflater().inflate(R.menu.about_menu, menu);
    return true;
  }

  @Override
  public boolean onPrepareOptionsMenu(Menu menu) {
    menu.findItem(R.id.action_perm_status).setEnabled(mMySettings.isPrivDaemonAlive());
    menu.findItem(R.id.action_dump_daemon_heap).setVisible(BuildConfig.DEBUG);
    return true;
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    if (item.getItemId() == R.id.action_dump_daemon_heap) {
      Utils.runInBg(() -> PrivDaemonHandler.getInstance().sendRequest(Commands.DUMP_HEAP));
      return true;
    }

    if (item.getItemId() == R.id.action_perm_status) {
      if (mMySettings.isPrivDaemonAlive()) {
        Utils.runInBg(this::showPermStatusDialog);
      } else {
        item.setEnabled(false);
      }
      return true;
    }
    return super.onOptionsItemSelected(item);
  }

  private void showPermStatusDialog() {
    PrivDaemonHandler daemonHandler = PrivDaemonHandler.getInstance();
    Object obj = daemonHandler.sendRequest(Commands.GET_PERM_STATUS);
    if (!(obj instanceof List<?>)) {
      return;
    }
    List<PermStatus> permStatusList = new ArrayList<>();
    for (Object item : (List<?>) obj) {
      permStatusList.add((PermStatus) item);
    }
    obj = daemonHandler.sendRequest(Commands.GET_APP_OP_STATUS);
    if (!(obj instanceof Integer)) {
      return;
    }
    int appOpsStatus = (int) obj;
    Utils.runInFg(() -> showPermStatusDialog(daemonHandler.getUid(), permStatusList, appOpsStatus));
  }

  private void showPermStatusDialog(int uid, List<PermStatus> permStatusList, int appOpStatus) {
    AboutPrivilegesDialogBinding b = AboutPrivilegesDialogBinding.inflate(getLayoutInflater());
    b.uidV.setText(String.valueOf(uid));

    b.opToDefModeV.setImageResource(getIcon(appOpStatus, Commands.OP_TO_DEF_MODE_WORKS));
    b.opToSwV.setImageResource(getIcon(appOpStatus, Commands.OP_TO_SWITCH_WORKS));
    b.opToNameV.setImageResource(getIcon(appOpStatus, Commands.OP_TO_NAME_WORKS));
    b.opNumConsistentV.setImageResource(getIcon(appOpStatus, Commands.OP_NUM_CONSISTENT));

    b.recyclerV.setAdapter(new AboutPrivilegesAdapter(permStatusList));
    b.recyclerV.setLayoutManager(new LinearLayoutManager(this));
    b.recyclerV.addItemDecoration(new DividerItemDecoration(this, DividerItemDecoration.VERTICAL));

    Builder builder =
        new Builder(this).setTitle(R.string.perm_status_menu_item).setView(b.getRoot());
    new AlertDialogFragment(builder.create()).show(this, "PERM_STATUS", false);
  }

  private @DrawableRes int getIcon(int appOpStatus, int type) {
    return ((appOpStatus & type) != 0) ? R.drawable.tick : R.drawable.cross;
  }
}
