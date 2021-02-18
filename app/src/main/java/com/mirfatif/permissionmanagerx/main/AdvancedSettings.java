package com.mirfatif.permissionmanagerx.main;

import android.annotation.SuppressLint;
import android.os.SystemClock;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import android.view.View;
import android.widget.CheckBox;
import android.widget.EditText;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.widget.AppCompatSpinner;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;

class AdvancedSettings {

  private static final String TAG = "AdvancedSettings";

  private final MainActivity mA;
  private final MySettings mMySettings = MySettings.getInstance();
  private final AdvancedSettingsFlavor mAdvancedSettingsFlavor;

  private final View dialogLayout;
  private final CheckBox useHiddenAPIsView;
  private final CheckBox dexInTmpDirView;
  private final EditText adbPortView;
  private final CheckBox useSocketView;
  private final AppCompatSpinner daemonUidSpinner;
  private final AppCompatSpinner daemonContextSpinner;
  private final View daemonUidListArrow;
  private final View daemonContextListArrow;
  private final EditText suExePathView;

  private final List<String> spinnerUids, spinnerContexts;
  private final int uidSelectedPos, contextSelectedPos;

  private final boolean useHiddenAPIs = mMySettings.useHiddenAPIs();
  private final boolean dexInTmpDir = mMySettings.dexInTmpDir();
  private final String adbPort = String.valueOf(mMySettings.getAdbPort());
  private final boolean useSocket = mMySettings.useSocket();
  private final String suExePath = mMySettings.getSuExePath();

  @SuppressLint("InflateParams")
  private AdvancedSettings(MainActivity activity) {
    mA = activity;

    dialogLayout = mA.getLayoutInflater().inflate(R.layout.advanced_settings_alert_dialog, null);

    useHiddenAPIsView = dialogLayout.findViewById(R.id.use_hidden_apis);
    dexInTmpDirView = dialogLayout.findViewById(R.id.dex_tmp_dir);
    adbPortView = dialogLayout.findViewById(R.id.adb_port);
    useSocketView = dialogLayout.findViewById(R.id.use_socket);
    daemonUidSpinner = dialogLayout.findViewById(R.id.daemon_uid_list);
    daemonContextSpinner = dialogLayout.findViewById(R.id.daemon_context_list);

    daemonUidListArrow = dialogLayout.findViewById(R.id.daemon_uid_list_arrow);
    daemonContextListArrow = dialogLayout.findViewById(R.id.daemon_context_list_arrow);
    suExePathView = dialogLayout.findViewById(R.id.su_exe_path);

    spinnerUids = Arrays.asList(mA.getResources().getStringArray(R.array.daemon_uids));
    spinnerContexts = Arrays.asList(mA.getResources().getStringArray(R.array.daemon_contexts));

    int uidResId = R.string.daemon_uid_system;
    if (mMySettings.getDaemonUid() == 0) {
      uidResId = R.string.daemon_uid_root;
    } else if (mMySettings.getDaemonUid() == 2000) {
      uidResId = R.string.daemon_uid_adb;
    }
    uidSelectedPos = spinnerUids.indexOf(getString(uidResId));

    int contextResId = R.string.daemon_context_shell;
    if (mMySettings.getDaemonContext().equals(MySettings.CONTEXT_DEFAULT)) {
      contextResId = R.string.daemon_context_default;
    }
    contextSelectedPos = spinnerContexts.indexOf(getString(contextResId));

    mAdvancedSettingsFlavor = new AdvancedSettingsFlavor(mA, dialogLayout);
  }

  private void show() {
    Builder builder =
        new Builder(mA)
            .setTitle(R.string.advanced_settings_menu_item)
            .setView(dialogLayout)
            .setPositiveButton(R.string.save, (d, which) -> saveSettings())
            .setNegativeButton(android.R.string.cancel, null);

    AlertDialog dialog;
    if (mMySettings.isRootGranted() && !mMySettings.isAdbConnected()) {
      builder.setNeutralButton(
          R.string.switch_to_adb, (d, w) -> Utils.runInBg(() -> switchToAdb(false)));
      dialog = builder.create();
      Utils.removeButtonPadding(dialog);
    } else {
      dialog = builder.create();
    }

    new AlertDialogFragment(dialog).show(mA, "ADVANCED_SETTINGS", false);

    daemonUidListArrow.setOnClickListener(v -> daemonUidSpinner.performClick());
    daemonContextListArrow.setOnClickListener(v -> daemonContextSpinner.performClick());
    useHiddenAPIsView.setChecked(useHiddenAPIs);
    dexInTmpDirView.setChecked(dexInTmpDir);
    adbPortView.setText(adbPort);
    adbPortView.addTextChangedListener(new PortNumberWatcher());
    useSocketView.setChecked(useSocket);
    daemonUidSpinner.setSelection(uidSelectedPos);
    daemonContextSpinner.setSelection(contextSelectedPos);
    suExePathView.setText(suExePath);
    suExePathView.addTextChangedListener(new SuPathWatcher());
  }

  private void saveSettings() {
    boolean restartDaemon = false, switchToAdb = false;
    if (dexInTmpDir != dexInTmpDirView.isChecked()) {
      mMySettings.setDexInTmpDir(dexInTmpDirView.isChecked());
      restartDaemon = true;
    }

    String newPort = adbPortView.getText() == null ? null : adbPortView.getText().toString().trim();
    if (!TextUtils.isEmpty(newPort) && !adbPort.equals(newPort)) {
      int port = Integer.parseInt(newPort);
      if (port > 65535 || port <= 0) {
        Utils.showToast(R.string.bad_port_number);
      } else {
        mMySettings.setAdbPort(port);
        restartDaemon = true;
        switchToAdb = true;
      }
    }

    if (useSocket != useSocketView.isChecked()) {
      mMySettings.setUseSocket(useSocketView.isChecked());
      restartDaemon = true;
    }

    int uidSelectedPosNew = daemonUidSpinner.getSelectedItemPosition();
    if (uidSelectedPos != uidSelectedPosNew) {
      String newSelection = spinnerUids.get(uidSelectedPosNew);
      int uid = 1000;
      if (newSelection.equals(getString(R.string.daemon_uid_root))) {
        uid = 0;
      } else if (newSelection.equals(getString(R.string.daemon_uid_adb))) {
        uid = 2000;
      }
      mMySettings.setDaemonUid(uid);
      restartDaemon = true;
    }

    int contextSelectedPosNew = daemonContextSpinner.getSelectedItemPosition();
    if (contextSelectedPos != contextSelectedPosNew) {
      String newSelection = spinnerContexts.get(contextSelectedPosNew);
      String context = MySettings.CONTEXT_SHELL;
      if (newSelection.equals(getString(R.string.daemon_context_default))) {
        context = MySettings.CONTEXT_DEFAULT;
      }
      mMySettings.setDaemonContext(context);
      restartDaemon = true;
    }

    String suExePathNew =
        suExePathView.getText() == null ? null : suExePathView.getText().toString().trim();
    if (TextUtils.isEmpty(suExePathNew)) {
      if (!TextUtils.isEmpty(suExePath)) {
        mMySettings.setSuExePath(null);
        restartDaemon = true;
      }
    } else if (!suExePathNew.equals(suExePath)) {
      if (!isExecutableFile(suExePathNew)) {
        Utils.showToast(R.string.bad_path);
      } else {
        mMySettings.setSuExePath(suExePathNew);
        restartDaemon = true;
      }
    }

    mAdvancedSettingsFlavor.saveSettings(restartDaemon, switchToAdb);

    if (useHiddenAPIs != useHiddenAPIsView.isChecked()) {
      saveHiddenAPIsSettings(useHiddenAPIsView.isChecked(), restartDaemon, switchToAdb);
    } else {
      restartDaemon(restartDaemon, switchToAdb);
    }
  }

  private void restartDaemon(boolean restartDaemon, boolean switchToAdb) {
    if (mMySettings.isRootGranted() && switchToAdb) {
      Utils.runInBg(() -> switchToAdb(restartDaemon));
    } else if (restartDaemon) {
      mA.restartPrivDaemon(!switchToAdb);
    }
  }

  private void saveHiddenAPIsSettings(
      boolean useHiddenAPIs, boolean restartDaemon, boolean switchToAdb) {
    if (useHiddenAPIs) {
      mMySettings.setUseHiddenAPIs(true);

      // Restart daemon to make sure that read AppOps permission is granted
      restartDaemon(true, switchToAdb);
      return;
    }

    final boolean[] doRestartDaemon = {restartDaemon};
    AlertDialog dialog =
        new Builder(mA)
            .setPositiveButton(
                R.string.yes,
                (d, which) -> {
                  mMySettings.setUseHiddenAPIs(false);
                  doRestartDaemon[0] = true; // Start daemon if not running
                })
            .setNegativeButton(R.string.no, null)
            .setTitle(R.string.hidden_apis)
            .setMessage(R.string.hidden_apis_confirmation)
            .create();
    new AlertDialogFragment(dialog)
        .setOnDismissListener(d -> restartDaemon(doRestartDaemon[0], switchToAdb))
        .show(mA, "HIDDEN_APIS_CONFIRM", false);
  }

  private void switchToAdb(boolean restartDaemon) {
    if (Utils.checkAdb(false)) {
      restartDaemonWithAdb();
      return;
    }

    Process process = Utils.runCommand(TAG, true, Utils.getSu());
    if (process == null) {
      Utils.showToast(R.string.adb_switch_fail);
    } else {
      Utils.runInBg(
          () -> {
            try (InputStreamReader reader = new InputStreamReader(process.getInputStream())) {
              Utils.readProcessLog(new BufferedReader(reader), TAG);
            } catch (IOException e) {
              e.printStackTrace();
            }
          });

      Log.i(TAG, "Sending ADB switch commands");

      PrintWriter writer = new PrintWriter(process.getOutputStream(), true);
      writer.println("settings put global adb_enabled 0");
      writer.println("stop adbd");
      writer.println("setprop service.adb.tcp.port " + mMySettings.getAdbPort());
      SystemClock.sleep(2000);
      writer.println("settings put global adb_enabled 1");
      writer.println("exec start adbd");
      writer.close();

      SystemClock.sleep(5000);
    }

    if (Utils.checkAdb(true)) {
      restartDaemonWithAdb();
    } else if (restartDaemon) {
      restartDaemon(true, false);
    }
  }

  private void restartDaemonWithAdb() {
    Log.i(TAG, "Restarting daemon");
    mA.restartPrivDaemon(false);
    Utils.runInFg(
        () -> {
          mA.showSnackBar(getString(R.string.connected_to_adb), 5000);
          mA.setNavigationMenu(); // To check Adb CheckBox
        });
  }

  private String getString(int resId) {
    return App.getContext().getString(resId);
  }

  private boolean isExecutableFile(CharSequence path) {
    File suFile = new File(path.toString());
    return suFile.isFile() && suFile.canExecute();
  }

  private class PortNumberWatcher implements TextWatcher {
    @Override
    public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

    @Override
    public void onTextChanged(CharSequence s, int start, int before, int count) {
      if (TextUtils.isEmpty(s)) {
        return;
      }
      int port = Integer.parseInt(s.toString().trim());
      if (port > 65535 || port <= 0) {
        adbPortView.setError(getString(R.string.bad_port_number));
      }
    }

    @Override
    public void afterTextChanged(Editable s) {}
  }

  private class SuPathWatcher implements TextWatcher {

    @Override
    public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

    @Override
    public void onTextChanged(CharSequence s, int start, int before, int count) {
      if (!TextUtils.isEmpty(s) && !isExecutableFile(s)) {
        suExePathView.setError(getString(R.string.bad_path));
      }
    }

    @Override
    public void afterTextChanged(Editable s) {}
  }

  static void showDialog(MainActivity mainActivity) {
    new AdvancedSettings(mainActivity).show();
  }
}
