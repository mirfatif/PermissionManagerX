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
import android.widget.Toast;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.widget.AppCompatSpinner;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;

class AdvancedSettings {

  private static final String TAG = "AdvancedSettings";

  private final MainActivity mA;
  private final MySettings mMySettings = MySettings.getInstance();

  private final View dialogLayout;
  private final CheckBox useHiddenAPIsView;
  private final CheckBox dexInTmpDirView;
  private final EditText adbPortView;
  private final CheckBox useSocketView;
  private final AppCompatSpinner daemonUidSpinner;
  private final AppCompatSpinner daemonContextSpinner;
  private final View daemonUidListArrow;
  private final View daemonContextListArrow;

  private final List<String> spinnerUids, spinnerContexts;
  private final int uidSelectedPos, contextSelectedPos;

  private final boolean useHiddenAPIs = mMySettings.useHiddenAPIs();
  private final boolean dexInTmpDir = mMySettings.dexInTmpDir();
  private final String adbPort = String.valueOf(mMySettings.getAdbPort());
  private final boolean useSocket = mMySettings.useSocket();

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
  }

  private void show() {
    daemonUidListArrow.setOnClickListener(v -> daemonUidSpinner.performClick());
    daemonContextListArrow.setOnClickListener(v -> daemonContextSpinner.performClick());
    useHiddenAPIsView.setChecked(useHiddenAPIs);
    dexInTmpDirView.setChecked(dexInTmpDir);
    adbPortView.setText(adbPort);
    adbPortView.addTextChangedListener(new PortNumberWatcher());
    useSocketView.setChecked(useSocket);
    daemonUidSpinner.setSelection(uidSelectedPos);
    daemonContextSpinner.setSelection(contextSelectedPos);

    Builder builder =
        new Builder(mA)
            .setTitle(R.string.advanced_settings_menu_item)
            .setView(dialogLayout)
            .setPositiveButton(R.string.save, (d, which) -> saveSettings())
            .setNegativeButton(android.R.string.cancel, null);

    AlertDialog dialog;
    if (mMySettings.isRootGranted() && !mMySettings.isAdbConnected()) {
      builder.setNeutralButton(R.string.switch_to_adb, (d, w) -> Utils.runInBg(this::switchToAdb));
      dialog = builder.create();
      Utils.removeButtonPadding(dialog);
    } else {
      dialog = builder.create();
    }

    new AlertDialogFragment(dialog).show(mA, "ADVANCED_SETTINGS", false);
  }

  private void saveSettings() {
    boolean restartDaemon = false, switchToAdb = false;
    if (useHiddenAPIs != useHiddenAPIsView.isChecked()) {
      restartDaemon = saveHiddenAPIsSettings(useHiddenAPIsView.isChecked());
    }

    if (dexInTmpDir != dexInTmpDirView.isChecked()) {
      mMySettings.setDexInTmpDir(dexInTmpDirView.isChecked());
      restartDaemon = true;
    }

    String adbPortNew = adbPortView.getText().toString().trim();
    if (!TextUtils.isEmpty(adbPortNew) && !adbPort.equals(adbPortNew)) {
      int port = Integer.parseInt(adbPortNew);
      if (port > 65535 || port <= 0) {
        Toast.makeText(App.getContext(), R.string.bad_port_number, Toast.LENGTH_LONG).show();
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

    if (mMySettings.isRootGranted() && switchToAdb) {
      Utils.runInBg(this::switchToAdb);
    } else if (restartDaemon) {
      mA.restartPrivDaemon(!switchToAdb);
    }
  }

  private boolean saveHiddenAPIsSettings(boolean useHiddenAPIs) {
    if (useHiddenAPIs) {
      mMySettings.setUseHiddenAPIs(true);

      // make sure read AppOps permission is granted
      return true;
    }

    AlertDialog dialog =
        new Builder(mA)
            .setPositiveButton(
                R.string.yes,
                (d, which) -> {
                  mMySettings.setUseHiddenAPIs(false);
                  mA.restartPrivDaemon(true); // Start daemon if not running
                })
            .setNegativeButton(R.string.no, null)
            .setTitle(R.string.hidden_apis)
            .setMessage(R.string.hidden_apis_confirmation)
            .create();
    new AlertDialogFragment(dialog).show(mA, "HIDDEN_APIS_CONFIRM", false);
    return false;
  }

  private void switchToAdb() {
    if (Utils.checkAdb(false)) {
      restartDaemon();
      return;
    }

    Process process = Utils.runCommand(TAG, true, "su");
    if (process == null) {
      Utils.runInFg(
          () ->
              Toast.makeText(App.getContext(), R.string.adb_switch_fail, Toast.LENGTH_LONG).show());
      return;
    }

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
    if (Utils.checkAdb(true)) {
      restartDaemon();
    }
  }

  private void restartDaemon() {
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

  static void showDialog(MainActivity mainActivity) {
    new AdvancedSettings(mainActivity).show();
  }
}
