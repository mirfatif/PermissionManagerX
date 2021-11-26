package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.ROOT_DAEMON;
import static com.mirfatif.permissionmanagerx.util.Utils.UID_ROOT;
import static com.mirfatif.permissionmanagerx.util.Utils.UID_SHELL;
import static com.mirfatif.permissionmanagerx.util.Utils.UID_SYSTEM;

import android.os.Bundle;
import android.os.SystemClock;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.fragment.app.FragmentManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.databinding.AdvSettingsDialogBinding;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.ui.base.BottomSheetDialogFrag;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.io.File;
import java.util.Arrays;
import java.util.List;

public class AdvSettingsDialogFrag extends BottomSheetDialogFrag {

  private static final String TAG = "AdvSettingsDialogFrag";

  public static void show(FragmentManager fm) {
    AdvSettingsDialogFrag frag = new AdvSettingsDialogFrag();
    frag.show(fm, "ADVANCED_SETTINGS");
  }

  private AdvSettingsDialogBinding mB;
  private AdvancedSettingsFlavor mAdvancedSettingsFlavor;

  private List<String> spinnerUids, spinnerContexts;
  private int uidSelectedPos, contextSelectedPos;

  private final boolean useHiddenAPIs = SETTINGS.useHiddenAPIs();
  private final boolean dexInTmpDir = SETTINGS.dexInTmpDir();
  private final String adbPort = String.valueOf(SETTINGS.getAdbPort());
  private final boolean useSocket = SETTINGS.useSocket();
  private final String suExePath = SETTINGS.getSuExePath();

  @Nullable
  @Override
  public View onCreateView(
      @NonNull LayoutInflater inflater,
      @Nullable ViewGroup container,
      @Nullable Bundle savedInstanceState) {
    mB = AdvSettingsDialogBinding.inflate(mA.getLayoutInflater(), container, container != null);

    spinnerUids = Arrays.asList(mA.getResources().getStringArray(R.array.daemon_uids));
    spinnerContexts = Arrays.asList(mA.getResources().getStringArray(R.array.daemon_contexts));

    int uidResId = R.string.daemon_uid_system;
    if (SETTINGS.getDaemonUid() == UID_ROOT) {
      uidResId = R.string.daemon_uid_root;
    } else if (SETTINGS.getDaemonUid() == UID_SHELL) {
      uidResId = R.string.daemon_uid_adb;
    }
    uidSelectedPos = spinnerUids.indexOf(getString(uidResId));

    int contextResId = R.string.daemon_context_shell;
    if (SETTINGS.getDaemonContext().equals(MySettings.CONTEXT_DEFAULT)) {
      contextResId = R.string.daemon_context_default;
    }
    contextSelectedPos = spinnerContexts.indexOf(getString(contextResId));

    mAdvancedSettingsFlavor = new AdvancedSettingsFlavor((MainActivity) mA, mB);

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

    mB.posButton.setOnClickListener(
        v -> {
          saveSettings();
          dismiss();
        });

    mB.negButton.setOnClickListener(v -> dismiss());
    if (SETTINGS.isRootGranted() && !SETTINGS.isAdbConnected()) {
      mB.neutralButton.setVisibility(View.VISIBLE);
      mB.neutralButton.setOnClickListener(
          v -> {
            Utils.runInBg(() -> switchToAdb(false));
            dismiss();
          });
    }

    return mB.getRoot();
  }

  private static final int MIN_PORT = 1;
  private static final int MAX_PORT = 65535;

  private void saveSettings() {
    boolean restartDaemon = false, switchToAdb = false;
    if (dexInTmpDir != mB.dexTmpDir.isChecked()) {
      SETTINGS.setDexInTmpDir(mB.dexTmpDir.isChecked());
      restartDaemon = true;
    }

    String newPort = mB.adbPort.getText() == null ? null : mB.adbPort.getText().toString().trim();
    if (newPort != null && !TextUtils.isEmpty(newPort) && !adbPort.equals(newPort)) {
      int port;
      try {
        port = Integer.parseInt(newPort);
      } catch (NumberFormatException ignored) {
        port = -1;
      }
      if (port > MAX_PORT || port < MIN_PORT) {
        Utils.showToast(R.string.bad_port_number);
      } else {
        SETTINGS.setAdbPort(port);
        restartDaemon = true;
        switchToAdb = true;
      }
    }

    if (useSocket != mB.useSocket.isChecked()) {
      SETTINGS.setUseSocket(mB.useSocket.isChecked());
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
      SETTINGS.setDaemonUid(uid);
      restartDaemon = true;
    }

    int contextSelectedPosNew = mB.daemonContextList.getSelectedItemPosition();
    if (contextSelectedPos != contextSelectedPosNew) {
      String newSelection = spinnerContexts.get(contextSelectedPosNew);
      String context = MySettings.CONTEXT_SHELL;
      if (newSelection.equals(getString(R.string.daemon_context_default))) {
        context = MySettings.CONTEXT_DEFAULT;
      }
      SETTINGS.setDaemonContext(context);
      restartDaemon = true;
    }

    String suExePathNew =
        mB.suExePath.getText() == null ? null : mB.suExePath.getText().toString().trim();
    if (TextUtils.isEmpty(suExePathNew)) {
      if (!TextUtils.isEmpty(suExePath)) {
        SETTINGS.setSuExePath(null);
        restartDaemon = true;
      }
    } else if (suExePathNew != null && !suExePathNew.equals(suExePath)) {
      if (!isExecutableFile(suExePathNew)) {
        Utils.showToast(R.string.bad_path);
      } else {
        SETTINGS.setSuExePath(suExePathNew);
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
    if (SETTINGS.isRootGranted() && switchToAdb) {
      Utils.runInBg(() -> switchToAdb(restartDaemon));
    } else if (restartDaemon) {
      ((MainActivity) mA).restartPrivDaemon(!switchToAdb, true);
    }
  }

  private void saveHiddenAPIsSettings(
      boolean useHiddenAPIs, boolean restartDaemon, boolean switchToAdb) {
    if (useHiddenAPIs) {
      SETTINGS.setUseHiddenAPIs(true);

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
                  SETTINGS.setUseHiddenAPIs(false);
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
    ROOT_DAEMON.sendCommand("run settings put global adb_enabled 0");
    ROOT_DAEMON.sendCommand("run stop adbd");
    ROOT_DAEMON.sendCommand("run setprop service.adb.tcp.port " + SETTINGS.getAdbPort());
    SystemClock.sleep(2000);
    ROOT_DAEMON.sendCommand("run settings put global adb_enabled 1");
    ROOT_DAEMON.sendCommand("run start adbd");
    SystemClock.sleep(5000);

    if (Utils.checkAdb(true)) {
      restartDaemonWithAdb();
    } else if (restartDaemon) {
      restartDaemon(true, false);
    }
  }

  private void restartDaemonWithAdb() {
    Log.i(TAG, "Restarting daemon");
    ((MainActivity) mA).restartPrivDaemon(false, true);
    Utils.runInFg(
        mA,
        () -> {
          ((MainActivity) mA).showSnackBar(getString(R.string.connected_to_adb), 5000);
          ((MainActivity) mA).setBoxesChecked();
        });
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
      int port;
      try {
        port = Integer.parseInt(s.toString().trim());
      } catch (NumberFormatException ignored) {
        port = -1;
      }
      if (port > MAX_PORT || port < MIN_PORT) {
        mB.adbPort.setError(getString(R.string.bad_port_number), null);
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
        mB.suExePath.setError(getString(R.string.bad_path), null);
      }
    }

    @Override
    public void afterTextChanged(Editable s) {}
  }
}
