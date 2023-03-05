package com.mirfatif.permissionmanagerx.about;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.net.Uri;
import android.util.TypedValue;
import android.view.View;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.content.FileProvider;
import androidx.fragment.app.FragmentActivity;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.ActivityCrashReportBinding;
import com.mirfatif.permissionmanagerx.fwk.CrashReportActivityM;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.LogUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;

public class CrashReportActivity {

  private static final String TAG = "CrashReportActivity";

  private final CrashReportActivityM mA;

  public CrashReportActivity(CrashReportActivityM activity) {
    mA = activity;
  }

  private ActivityCrashReportBinding mB;

  public void onCreated() {
    mB = ActivityCrashReportBinding.inflate(mA.getLayoutInflater());
    mA.setContentView(mB.getRoot());

    ActionBar actionBar = mA.getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(R.string.crash_report);
    }

    new LiveTasksQueueTyped<>(mA, this::getFileContents).onUiWith(this::setFileContents).start();
  }

  public void onPause() {
    if (mAskedToSendEmail) {
      mPausedForEmailSendWaiting = true;
    }
  }

  public void onResume() {
    if (mAskedToSendEmail && mPausedForEmailSendWaiting) {
      mAskedToSendEmail = mPausedForEmailSendWaiting = false;
      askToDeleteOldFile();
    }
  }

  private String getFileContents() {

    NotificationManagerCompat.from(App.getCxt())
        .cancel(ApiUtils.getInt(R.integer.channel_crash_report));

    File logFile = LogUtils.getCrashLogFile();
    if (!logFile.exists()) {
      UiUtils.showToast(R.string.crash_log_file_not_exists_toast);
      return null;
    }

    try (InputStream is = new FileInputStream(logFile)) {
      ByteArrayOutputStream os = new ByteArrayOutputStream();
      if (Utils.copyStream(TAG, is, os)) {
        String text = new String(os.toByteArray(), StandardCharsets.UTF_8);

        BufferedReader reader = new BufferedReader(new StringReader(text));
        int lineCount = 0;
        while (reader.readLine() != null) {
          lineCount++;
        }

        if (lineCount <= LogUtils.CRASH_FILE_HEADER_LINES) {
          UiUtils.showToast(R.string.crash_log_file_not_exists_toast);
          return null;
        } else {
          return text;
        }
      }
    } catch (IOException e) {
      MyLog.e(TAG, "getFileContents", e);
    }

    UiUtils.showToast(R.string.crash_log_file_read_failed_toast);
    return null;
  }

  private void setFileContents(String fileContents) {
    if (fileContents == null) {
      mA.finishAfterTransition();
      return;
    }

    mB.progCont.setVisibility(View.GONE);
    mB.reportCont.setVisibility(View.VISIBLE);

    mB.contentV.setText(fileContents);

    float size = mB.emailDesc.getTextSize();
    mB.emailDesc.setTextSize(TypedValue.COMPLEX_UNIT_PX, size * 80 / 100);

    size = mB.sendEmailButton.getTextSize();
    mB.sendEmailButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, size * 80 / 100);

    mB.submitButton.setOnClickListener(
        v -> {
          mB.submitButton.setText(R.string.submitting_report_button);
          mB.submitButton.setEnabled(false);

          new LiveTasksQueueTyped<>(mA, () -> submit(fileContents))
              .onUiWith(this::handleSubmitResult)
              .start();
        });

    mB.sendEmailButton.setOnClickListener(v -> sendEmail());
  }

  private static synchronized boolean submit(String fileContents) {
    try (ServerConnection conn = new ServerConnection()) {
      if (!conn.write(fileContents)) {
        return false;
      }
    } catch (IOException e) {
      MyLog.e(TAG, "submit", e);
      return false;
    }

    File logFile = LogUtils.getCrashLogFile();
    if (!logFile.delete()) {
      MyLog.e(TAG, "submit", "Failed to delete " + logFile.getAbsolutePath());
    }

    LogUtils.createCrashLogFile();

    return true;
  }

  private static final String URL = "https://api.mirfatif.com/crash-report?app=PMX";

  private static class ServerConnection implements AutoCloseable {

    private final HttpURLConnection conn;

    private ServerConnection() throws IOException {
      conn = (HttpURLConnection) new URL(URL).openConnection();
      conn.setConnectTimeout(60000);
      conn.setReadTimeout(60000);
      conn.setDoOutput(true);
      conn.setRequestMethod("PUT");
      conn.setRequestProperty("Content-Type", "text/plain; charset=UTF-8");
      conn.connect();
    }

    public void close() {
      if (conn != null) {
        conn.disconnect();
      }
    }

    private boolean write(String fileContents) throws IOException {
      OutputStream os = conn.getOutputStream();
      os.write(fileContents.getBytes(StandardCharsets.UTF_8));
      os.flush();
      os.close();

      int code = conn.getResponseCode();
      if (code == HttpURLConnection.HTTP_CREATED || code == HttpURLConnection.HTTP_OK) {
        return true;
      }

      MyLog.e(TAG, "write", "Response code: " + code + " (" + conn.getResponseMessage() + ")");
      return false;
    }
  }

  private void handleSubmitResult(boolean done) {
    if (done) {
      sayThankYou();
      mA.finishAfterTransition();
    } else {
      mB.submitButton.setText(R.string.submit_report_button);
      mB.submitButton.setEnabled(true);
      UiUtils.showToast(R.string.submit_crash_report_failed_toast);
    }
  }

  private void sayThankYou() {
    UiUtils.showToast(R.string.thank_you);
  }

  private boolean mAskedToSendEmail = false, mPausedForEmailSendWaiting = true;

  private void sendEmail() {
    Uri uri =
        FileProvider.getUriForFile(
            App.getCxt(), BuildConfig.LOG_FILE_PROVIDER, LogUtils.getCrashLogFile());

    Intent intent =
        new Intent(Intent.ACTION_SEND)
            .putExtra(Intent.EXTRA_EMAIL, new String[] {ApiUtils.getString(R.string.email_address)})
            .putExtra(Intent.EXTRA_SUBJECT, "PMX - Crash Report")
            .putExtra(Intent.EXTRA_TEXT, "Find attachment.")
            .setDataAndType(uri, "text/plain")
            .putExtra(Intent.EXTRA_STREAM, uri)
            .setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);

    try {
      mA.startActivity(intent);
      mAskedToSendEmail = true;
    } catch (ActivityNotFoundException e) {
      UiUtils.showToast(R.string.no_email_app_installed);
    }
  }

  private void askToDeleteOldFile() {
    sayThankYou();

    AlertDialog dialog =
        new AlertDialog.Builder(mA)
            .setTitle(R.string.delete_crash_report_file_title)
            .setMessage(R.string.crash_file_delete_confirmation)
            .setPositiveButton(android.R.string.ok, (d, w) -> BgRunner.execute(this::deleteOldFile))
            .setNegativeButton(android.R.string.cancel, null)
            .create();
    AlertDialogFragment.show(mA, dialog, "DELETE_OLD_CRASH_FILE");
  }

  private void deleteOldFile() {
    File logFile = LogUtils.getCrashLogFile();
    if (!logFile.delete()) {
      MyLog.e(TAG, "submit", "Failed to delete " + logFile.getAbsolutePath());
    }

    LogUtils.createCrashLogFile();
    mA.finish();
  }

  public static void start(FragmentActivity act) {
    act.startActivity(new Intent(App.getCxt(), CrashReportActivityM.class));
  }
}
