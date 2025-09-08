package com.mirfatif.permissionmanagerx.about

import android.content.Intent
import android.view.View
import androidx.annotation.MainThread
import androidx.annotation.WorkerThread
import androidx.appcompat.app.AlertDialog
import androidx.core.app.NotificationManagerCompat
import androidx.fragment.app.FragmentActivity
import androidx.lifecycle.lifecycleScope
import com.mirfatif.permissionmanagerx.R
import com.mirfatif.permissionmanagerx.app.App
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment
import com.mirfatif.permissionmanagerx.databinding.ActivityCrashReportBinding
import com.mirfatif.permissionmanagerx.fwk.CrashReportActivityM
import com.mirfatif.permissionmanagerx.prefs.AppUpdate
import com.mirfatif.permissionmanagerx.util.ApiUtils
import com.mirfatif.permissionmanagerx.util.LogUtils
import com.mirfatif.permissionmanagerx.util.UiUtils
import com.mirfatif.privtasks.util.MyLog
import java.io.IOException
import java.lang.AutoCloseable
import java.net.HttpURLConnection
import java.net.URL
import java.nio.charset.StandardCharsets
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

class CrashReportActivity(private val mA: CrashReportActivityM) {
  private lateinit var mB: ActivityCrashReportBinding

  fun onCreated() {
    mB = ActivityCrashReportBinding.inflate(mA.layoutInflater)
    mA.setContentView(mB)

    mA.supportActionBar?.setTitle(R.string.crash_report)

    mA.lifecycleScope.launch(Dispatchers.Main) { setFileContents(readFileContents()) }
  }

  private data class Result(val updateResult: AppUpdate.AppUpdateResult?, val contents: String)

  private suspend fun readFileContents(): Result? =
      withContext(Dispatchers.IO) {
        NotificationManagerCompat.from(App.getCxt())
            .cancel(ApiUtils.getInt(R.integer.channel_crash_report))

        val logFile = LogUtils.getCrashLogFile()
        if (!logFile.exists()) {
          UiUtils.showToast(R.string.crash_log_file_not_exists_toast)
          return@withContext null
        }

        val text: String

        try {
          text = logFile.readText()
        } catch (e: IOException) {
          MyLog.e(TAG, "getFileContents", e)
          UiUtils.showToast(R.string.crash_log_file_read_failed_toast)
          return@withContext null
        }

        if (text.split('\n').size <= LogUtils.CRASH_FILE_HEADER_LINES) {
          UiUtils.showToast(R.string.crash_log_file_not_exists_toast)
          null
        } else {
          withContext(Dispatchers.Main) { mB.progMsg.setText(R.string.checking_for_app_update) }
          Result(AppUpdate.check(false), text)
        }
      }

  @MainThread
  private fun setFileContents(result: Result?) {
    if (result == null) {
      mA.finishAfterTransition()
      return
    }

    mB.reportCont.visibility = View.VISIBLE
    mB.contentV.text = result.contents

    handleAppUpdate(result)
  }

  @MainThread
  private fun handleAppUpdate(res: Result) {
    mB.progCont.visibility = View.GONE

    if (res.updateResult == null) {
      mB.submitButton.isEnabled = true

      mB.submitButton.setOnClickListener {
        mB.submitButton.setText(R.string.submitting_report_button)
        mB.submitButton.isEnabled = false

        mA.lifecycleScope.launch(Dispatchers.Main) {
          handleSubmitResult(withContext(Dispatchers.IO) { submit(res.contents) })
        }
      }
    } else if (res.updateResult.failed) {
      val builder =
          AlertDialog.Builder(mA)
              .setTitle(R.string.update)
              .setMessage(R.string.check_for_updates_failed_long)
              .setPositiveButton(R.string.retry_button) { d, w ->
                mB.progCont.visibility = View.VISIBLE
                mA.lifecycleScope.launch(Dispatchers.IO) {
                  val updateResult = AppUpdate.check(false)
                  withContext(Dispatchers.Main) {
                    handleAppUpdate(Result(updateResult, res.contents))
                  }
                }
              }
      AlertDialogFragment.show(mA, builder.create(), "APP_UPDATE")
    } else {
      AboutActivity.showAppUpdateDialog(mA, res.updateResult.version, false)
    }
  }

  @Synchronized
  @WorkerThread
  private fun submit(fileContents: String): Boolean {
    try {
      ServerConnection().use { conn ->
        if (!conn.write(fileContents)) {
          return false
        }
      }
    } catch (e: IOException) {
      MyLog.e(TAG, "submit", e)
      return false
    }

    val logFile = LogUtils.getCrashLogFile()
    if (!logFile.delete()) {
      MyLog.e(TAG, "submit", "Failed to delete " + logFile.absolutePath)
    }

    LogUtils.createCrashLogFile()

    return true
  }

  private class ServerConnection : AutoCloseable {
    private val conn =
        URL("https://api.mirfatif.com/crash-report?app=PMX").openConnection() as HttpURLConnection

    init {
      conn.setConnectTimeout(60000)
      conn.setReadTimeout(60000)
      conn.setDoOutput(true)
      conn.setRequestMethod("PUT")
      conn.setRequestProperty("Content-Type", "text/plain; charset=UTF-8")
      conn.connect()
    }

    override fun close() {
      conn.disconnect()
    }

    fun write(fileContents: String): Boolean {
      conn.getOutputStream().use { it.write(fileContents.toByteArray(StandardCharsets.UTF_8)) }

      val code = conn.getResponseCode()
      if (code == HttpURLConnection.HTTP_CREATED || code == HttpURLConnection.HTTP_OK) {
        return true
      }

      MyLog.e(TAG, "write", "Response code: " + code + " (" + conn.getResponseMessage() + ")")
      return false
    }
  }

  private fun handleSubmitResult(done: Boolean) {
    if (done) {
      UiUtils.showToast(R.string.thank_you)
      mA.finishAfterTransition()
    } else {
      mB.submitButton.setText(R.string.submit_report_button)
      mB.submitButton.isEnabled = true
      UiUtils.showToast(R.string.submit_crash_report_failed_toast)
    }
  }

  companion object {
    private const val TAG = "CrashReportActivity"

    @JvmStatic
    fun start(act: FragmentActivity) {
      act.startActivity(Intent(App.getCxt(), CrashReportActivityM::class.java))
    }
  }
}
