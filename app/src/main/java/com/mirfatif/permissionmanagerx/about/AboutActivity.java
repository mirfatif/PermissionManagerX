package com.mirfatif.permissionmanagerx.about;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.net.Uri;
import android.os.Debug;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.activity.result.contract.ActivityResultContracts.CreateDocument;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.ActivityAboutBinding;
import com.mirfatif.permissionmanagerx.fwk.AboutActivityM;
import com.mirfatif.permissionmanagerx.help.HelpActivity;
import com.mirfatif.permissionmanagerx.main.FeedbackDialogFrag;
import com.mirfatif.permissionmanagerx.main.FeedbackDialogFrag.FeedbackType;
import com.mirfatif.permissionmanagerx.prefs.AppUpdate;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.settings.SettingsActivity;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.svc.LogcatSvc;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.NotifUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.Util;
import com.mirfatif.privtasks.util.bg.SingleSchedTaskExecutor;
import java.io.File;
import java.io.IOException;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class AboutActivity {

  private static final String TAG = "AboutActivity";

  private final AboutActivityM mA;

  public AboutActivity(AboutActivityM activity) {
    mA = activity;
  }

  private ActivityAboutBinding mB;
  private ActivityResultLauncher<String> mNotifPermReqLauncher, mLoggingLauncher, mDumpLauncher;

  public void onCreated() {
    mB = ActivityAboutBinding.inflate(mA.getLayoutInflater());
    mA.setContentView(mB.getRoot());

    ActionBar actionBar = mA.getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(R.string.about_menu_item);
    }

    mB.version.setText(BuildConfig.VERSION_NAME);
    openWebUrl(mB.telegram, R.string.telegram_group_link);
    openWebUrl(mB.sourceCode, R.string.source_url);
    openWebUrl(mB.issues, R.string.issues_url);
    mB.rating.setOnClickListener(
        v -> FeedbackDialogFrag.show(FeedbackType.RATE, mA.getSupportFragmentManager()));
    mB.contact.setOnClickListener(v -> ApiUtils.sendMail(mA, null));
    setLogTitle(alreadyLogging() ? R.string.stop_logging : R.string.collect_logs);
    mB.logging.setOnClickListener(
        v -> {
          if (ApiUtils.hasNotifPerm()) {
            handleLogging(true);
          } else {
            NotifUtils.askForNotifPerm(mA, mNotifPermReqLauncher);
          }
        });
    mB.sendCrashReport.setOnClickListener(v -> CrashReportActivity.start(mA));

    openWebUrl(mB.privacyPolicy, R.string.privacy_policy_link);
    mB.checkUpdate.setOnClickListener(v -> checkForUpdates());
    mB.translate.setOnClickListener(
        v -> TransCreditsDialogFrag.show(mA.getSupportFragmentManager()));
    mB.shareApp.setOnClickListener(v -> sendShareIntent(mA));

    mB.paidFeatures.setOnClickListener(
        v -> HelpActivity.start(mA, getString(R.string.paid_features_href)));

    mNotifPermReqLauncher =
        mA.registerForActivityResult(
            new ActivityResultContracts.RequestPermission(), this::handleLogging);

    ActivityResultCallback<Uri> callback =
        logFile -> {
          if (logFile != null) {
            LogcatSvc.start(logFile);
            setLogTitle(R.string.stop_logging);
          }
        };

    mLoggingLauncher = mA.registerForActivityResult(new CreateDocument("text/plain"), callback);
  }

  private void openWebUrl(View view, int linkResId) {
    view.setOnClickListener(v -> ApiUtils.openWebUrl(mA, getString(linkResId)));
  }

  private void setLogTitle(int resId) {
    mB.loggingTitle.setText(resId);
  }

  private static boolean alreadyLogging() {
    return MySettings.INS.isDebug();
  }

  private void handleLogging(boolean notifPermGranted) {
    if (alreadyLogging()) {
      LogcatSvc.stopSvc();
      setLogTitle(R.string.collect_logs);
      Snackbar.make(mB.logging, R.string.logging_stopped, 5000).show();
      return;
    }

    if (notifPermGranted) {
      UiUtils.showToast(R.string.select_log_file);
      try {
        mLoggingLauncher.launch("PermissionManagerX_" + Util.getCurrDateTime(false, true) + ".log");
      } catch (ActivityNotFoundException ignored) {
        UiUtils.showToast(R.string.no_file_picker_installed);
      }
    }
  }

  private boolean mCheckForUpdateInProgress = false;

  private void checkForUpdates() {
    if (mCheckForUpdateInProgress) {
      return;
    }
    mCheckForUpdateInProgress = true;

    mB.checkUpdateSummary.setText(R.string.check_in_progress);

    new LiveTasksQueueTyped<>(mA, () -> AppUpdate.check(false))
        .onUiWith(this::handleAppUpdateResult)
        .start();
  }

  private void handleAppUpdateResult(AppUpdate.AppUpdateResult res) {
    int msg;
    boolean showDialog = false;

    if (res == null) {
      msg = R.string.app_is_up_to_date;
    } else if (res.failed) {
      msg = R.string.check_for_updates_failed;
    } else {
      msg = R.string.new_version_available;
      showDialog = true;
    }

    mB.checkUpdateSummary.setText(R.string.update_summary);

    if (showDialog) {
      Builder builder =
          new Builder(mA)
              .setTitle(R.string.update)
              .setMessage(getString(msg) + ": " + res.version)
              .setPositiveButton(
                  R.string.download, (d, w) -> ApiUtils.openWebUrl(mA, res.updateUrl))
              .setNegativeButton(R.string.cancel_button, null);
      AlertDialogFragment.show(mA, builder.create(), "APP_UPDATE");
    } else {
      UiUtils.showToast(msg);
    }

    mCheckForUpdateInProgress = false;
  }

  public static void sendShareIntent(Activity activity) {
    Intent intent = new Intent(Intent.ACTION_SEND).setType("text/plain");
    intent.putExtra(Intent.EXTRA_SUBJECT, getString(R.string.app_name));
    String text = getString(R.string.share_text, getString(R.string.source_url));
    activity.startActivity(Intent.createChooser(intent.putExtra(Intent.EXTRA_TEXT, text), null));
  }

  public boolean onCreateOptionsMenu(Menu menu) {
    mA.getMenuInflater().inflate(R.menu.about_menu, menu);
    return true;
  }

  public boolean onPrepareOptionsMenu(Menu menu) {
    menu.findItem(R.id.action_perm_status).setEnabled(DaemonHandler.INS.isDaemonAlive());
    menu.findItem(R.id.action_dump_daemon_heap).setVisible(BuildConfig.DEBUG);
    return true;
  }

  public boolean onOptionsItemSelected(MenuItem item) {
    if (item.getItemId() == R.id.action_dump_daemon_heap) {
      item.setEnabled(false);
      mDumpHeap = true;
      return true;
    }

    if (item.getItemId() == R.id.action_perm_status) {
      if (DaemonHandler.INS.isDaemonAlive()) {
        AlertDialogFragment.show(mA, null, TAG_PERM_STATUS);
      } else {
        item.setEnabled(false);
      }
      return true;
    }
    return false;
  }

  private boolean mDumpHeap = false;

  public void onDestroy() {
    if (mDumpHeap) {
      UiUtils.showToast("Heap dump will be taken after 10 seconds");
      SingleSchedTaskExecutor.schedule(
          AboutActivity::dumpHeap, 10, TimeUnit.SECONDS, TAG + "-HeapDump");
    }
  }

  private static final String CLASS = SettingsActivity.class.getName();
  private static final String TAG_PERM_STATUS = CLASS + ".PERM_STATUS";

  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    if (TAG_PERM_STATUS.equals(tag)) {
      return new PrivilegesDialog(mA).create(dialogFragment);
    }

    return null;
  }

  private static void dumpHeap() {
    UiUtils.showToast("Taking heap dump");

    boolean done = DaemonIface.INS.dumpHeap();

    System.gc();

    String directory = Objects.requireNonNull(App.getCxt().getExternalCacheDir()).getAbsolutePath();

    File dir = new File(directory);
    if (!dir.isDirectory()) {
      MyLog.e(TAG, "dumpHeap", directory + " is not a directory");
      done = false;
    }

    File file = new File(dir, "com.mirfatif.pmx.hprof");
    try {
      Debug.dumpHprofData(file.getAbsolutePath());
    } catch (IOException e) {
      MyLog.e(TAG, "dumpHeap", e);
      done = false;
    }

    UiUtils.showToast(done ? "Heap dump completed" : "Heap dump failed");
  }

  public static void start(Activity activity) {
    activity.startActivity(new Intent(App.getCxt(), AboutActivityM.class));
  }
}
