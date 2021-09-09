package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.util.Utils.UID_ROOT;
import static com.mirfatif.permissionmanagerx.util.Utils.UID_SHELL;
import static com.mirfatif.permissionmanagerx.util.Utils.UID_SYSTEM;

import android.os.SystemClock;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.AdvSettingsDialogBinding;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.io.File;
import java.util.Arrays;
import java.util.List;

class AdvancedSettings {

  private static final String TAG = "AdvancedSettings";

  private final MainActivity mA;
  private final MySettings mMySettings = MySettings.getInstance();
  private final AdvancedSettingsFlavor mAdvancedSettingsFlavor;

  private final List<String> spinnerUids, spinnerContexts;
  private final int uidSelectedPos, contextSelectedPos;

  private final boolean useHiddenAPIs = mMySettings.useHiddenAPIs();
  private final boolean dexInTmpDir = mMySettings.dexInTmpDir();
  private final String adbPort = String.valueOf(mMySettings.getAdbPort());
  private final boolean useSocket = mMySettings.useSocket();
  private final String suExePath = mMySettings.getSuExePath();

  private final AdvSettingsDialogBinding mB;

  AdvancedSettings(MainActivity activity) {
    mA = activity;
    mB = AdvSettingsDialogBinding.inflate(mA.getLayoutInflater());

    spinnerUids = Arrays.asList(mA.getResources().getStringArray(R.array.daemon_uids));
    spinnerContexts = Arrays.asList(mA.getResources().getStringArray(R.array.daemon_contexts));

    int uidResId = R.string.daemon_uid_system;
    if (mMySettings.getDaemonUid() == UID_ROOT) {
      uidResId = R.string.daemon_uid_root;
    } else if (mMySettings.getDaemonUid() == UID_SHELL) {
      uidResId = R.string.daemon_uid_adb;
    }
    uidSelectedPos = spinnerUids.indexOf(getString(uidResId));

    int contextResId = R.string.daemon_context_shell;
    if (mMySettings.getDaemonContext().equals(MySettings.CONTEXT_DEFAULT)) {
      contextResId = R.string.daemon_context_default;
    }
    contextSelectedPos = spinnerContexts.indexOf(getString(contextResId));

    mAdvancedSettingsFlavor = new AdvancedSettingsFlavor(mA, mB);

    mB.daemonUidListArrow.setOnClickListener(v -> mB.daemonUidList.performClick());
    mB.daemonContextListArrow.setOnClickListener(v -> mB.daemonContextList.performClick());
    mB.useHiddenApis.setChecked(useHiddenAPIs);
    mB.dexTmpDir.setChecked(dexInTmpDir);
    mB.adbPort.setText(adbPort);
    mB.adbPort.addTextChangedListener(new PortNumberWatcher());
    mB.useSocket.setChecked(useSocket);
    mB.daemonUidList.setSelection(uidSelectedPos);
    mB.daemonContextList.setSelection(contextSelectedPos);
    mB.suExePath.setText(suExePath);
    mB.suExePath.addTextChangedListener(new SuPathWatcher());
  }

  AlertDialog createDialog() {
    Builder builder =
        new Builder(mA)
            .setTitle(R.string.advanced_settings_menu_item)
            .setView(mB.getRoot())
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
    return dialog;
  }

  private static final int MIN_PORT = 1;
  private static final int MAX_PORT = 65535;

  private void saveSettings() {
    boolean restartDaemon = false, switchToAdb = false;
    if (dexInTmpDir != mB.dexTmpDir.isChecked()) {
      mMySettings.setDexInTmpDir(mB.dexTmpDir.isChecked());
      restartDaemon = true;
    }

    String newPort = mB.adbPort.getText() == null ? null : mB.adbPort.getText().toString().trim();
    if (newPort != null && !TextUtils.isEmpty(newPort) && !adbPort.equals(newPort)) {
      int port = Integer.parseInt(newPort);
      if (port > MAX_PORT || port < MIN_PORT) {
        Utils.showToast(R.string.bad_port_number);
      } else {
        mMySettings.setAdbPort(port);
        restartDaemon = true;
        switchToAdb = true;
      }
    }

    if (useSocket != mB.useSocket.isChecked()) {
      mMySettings.setUseSocket(mB.useSocket.isChecked());
      restartDaemon = true;
    }

    int uidSelectedPosNew = mB.daemonUidList.getSelectedItemPosition();
    if (uidSelectedPos != uidSelectedPosNew) {
      String newSelection = spinnerUids.get(uidSelectedPosNew);
      int uid = UID_SYSTEM;
      if (newSelection.equals(getString(R.string.daemon_uid_root))) {
        uid = UID_ROOT;
      } else if (newSelection.equals(getString(R.string.daemon_uid_adb))) {
        uid = UID_SHELL;
      }
      mMySettings.setDaemonUid(uid);
      restartDaemon = true;
    }

    int contextSelectedPosNew = mB.daemonContextList.getSelectedItemPosition();
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
        mB.suExePath.getText() == null ? null : mB.suExePath.getText().toString().trim();
    if (TextUtils.isEmpty(suExePathNew)) {
      if (!TextUtils.isEmpty(suExePath)) {
        mMySettings.setSuExePath(null);
        restartDaemon = true;
      }
    } else if (suExePathNew != null && !suExePathNew.equals(suExePath)) {
      if (!isExecutableFile(suExePathNew)) {
        Utils.showToast(R.string.bad_path);
      } else {
        mMySettings.setSuExePath(suExePathNew);
        restartDaemon = true;
      }
    }

    mAdvancedSettingsFlavor.saveSettings(restartDaemon, switchToAdb);

    if (useHiddenAPIs != mB.useHiddenApis.isChecked()) {
      saveHiddenAPIsSettings(mB.useHiddenApis.isChecked(), restartDaemon, switchToAdb);
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

    AlertDialogFragment.show(mA, dialog, "HIDDEN_APIS_CONFIRM")
        .setOnDismissListener(d -> restartDaemon(doRestartDaemon[0], switchToAdb));
  }

  private void switchToAdb(boolean restartDaemon) {
    if (Utils.checkAdb(false)) {
      restartDaemonWithAdb();
      return;
    }

    Log.i(TAG, "Sending ADB switch commands");
    NativeDaemon daemon = NativeDaemon.rootInstance();
    daemon.sendCommand("run settings put global adb_enabled 0");
    daemon.sendCommand("run stop adbd");
    daemon.sendCommand("run setprop service.adb.tcp.port " + mMySettings.getAdbPort());
    SystemClock.sleep(2000);
    daemon.sendCommand("run settings put global adb_enabled 1");
    daemon.sendCommand("run start adbd");
    SystemClock.sleep(5000);

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

  @SuppressWarnings("BooleanMethodIsAlwaysInverted")
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
      if (port > MAX_PORT || port < MIN_PORT) {
        mB.adbPort.setError(getString(R.string.bad_port_number));
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
        mB.suExePath.setError(getString(R.string.bad_path));
      }
    }

    @Override
    public void afterTextChanged(Editable s) {}
  }
}
